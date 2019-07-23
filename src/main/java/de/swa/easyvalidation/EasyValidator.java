package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.constraints.ConstraintRef;
import de.swa.easyvalidation.constraints.Permissions;
import de.swa.easyvalidation.groups.AndGroup;
import de.swa.easyvalidation.groups.ConstraintRefGroup;
import de.swa.easyvalidation.groups.ConstraintRefTopGroup;
import de.swa.easyvalidation.groups.ContentContraintGroup;
import de.swa.easyvalidation.groups.OrGroup;
import de.swa.easyvalidation.util.IndexedPropertyHelper;
import de.swa.easyvalidation.util.IndexedPropertyHelper.IndexInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.MethodDescriptor;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

public class EasyValidator {

    private static Logger log = LoggerFactory.getLogger(EasyValidator.class);

    private static String defaultMandatoryMessage = "error.validation.property.mandatory";
    private static String defaultImmutableMessage = "error.validation.property.immutable";
    private static String defaultContentMessage = "error.validation.property.content";

    private static final Map<PropertyDescriptor, GetterInfo> propertyToGetterReturnTypeCache = new HashMap<>();

    /**
     * Validates that the property exists for that class.
     *
     * @param property
     * @param clazz
     * @return
     */
    public static Class<?> validateProperty(final String property, final Class<?> clazz) {
        final GetterInfo cachedHit = propertyToGetterReturnTypeCache.get(new PropertyDescriptor(property, clazz));
        // log.debug("cachedHit for " + property + " " + cachedHit);
        return cachedHit != null ? cachedHit.getReturnType() : validatePropertyAndCache(property, clazz);
    }

    /**
     * E.g. "location.address.city" for class Article<br/>
     * 1st check: Article.location exist, cache ("location", Article.class) -> (getLocation(), Location.class)<br/>
     * 2nd check: Location.address exist, cache ("location.address", Article.class) -> (getAddress(), Address.class)
     * <br/>
     * 3rd check: Address.city exist, cache ("location.address.city", Article.class) -> (getCity(), String.class)
     */
    private static Class<?> validatePropertyAndCache(final String nestedProperty, final Class<?> clazz) {
        // Split a nested property into its parts; e.g. ["location", "address", "city"]
        final String[] propertyParts = nestedProperty.split("\\.");
        Class<?> propertyClass = clazz;
        String propertyKey = "";
        GetterInfo getterReturnType = null; // TODO not useful?! Cache Method instead ...
        for (final String propertyPart : propertyParts) {
            if (IndexedPropertyHelper.getIndexInfo(propertyPart) == null) {
                // process 'simple' property
                final Method getterMethod = getGetterMethodOrFail(propertyPart, propertyClass);
                getterReturnType = getGetterReturnType(getterMethod);
                propertyClass = getterReturnType.getReturnType();
            } else {
                // process property with index definitions (e.g. articles[0])
                final String propertyName = propertyPart.substring(0, propertyPart.indexOf('['));
                final Method getterMethod = getGetterMethodOrFail(propertyName, propertyClass);
                if (List.class.isAssignableFrom(getterMethod.getReturnType())) {
                    // Process list (e.g. for property articles[*])
                    // 1. get list field (-> articles)
                    final Field listField;
                    try {
                        listField = clazz.getDeclaredField(propertyName);
                    } catch (final NoSuchFieldException | SecurityException e) {
                        throw new IllegalArgumentException("Property " + propertyName + " is not a declared field of " + clazz);
                    }
                    // 2. get generic type (-> Article.class)
                    final ParameterizedType listType = (ParameterizedType) listField.getGenericType();
                    final Class<?> listClass = (Class<?>) listType.getActualTypeArguments()[0];
                    // 3. get getter method (-> getArticles())
                    getterReturnType = getGetterReturnType(getterMethod);
                    // 4. ignore return type 'List' and remember the type of its get(int) call (-> Article)
                    getterReturnType.setReturnType(listClass); // is this too tricky?
                    propertyClass = listClass;
                } else if (getterMethod.getReturnType().isArray()) {
                    // process array
                    final Class<?> arrayTypeClass = getterMethod.getReturnType().getComponentType();
                    getterReturnType = getGetterReturnType(getterMethod);
                    getterReturnType.setReturnType(arrayTypeClass); // is this too tricky?
                    propertyClass = arrayTypeClass;
                } else {
                    throw new IllegalArgumentException("Index definitions are only allowed for properties of type List or arrays: " + propertyPart);
                }
            }
            propertyKey += "." + propertyPart;
            propertyToGetterReturnTypeCache.put(new PropertyDescriptor(propertyKey.substring(1), clazz),
                    getterReturnType);
        }

        return propertyClass;
    }


    public static List<String> validateImmutableConditions(final Object originalObject, final Object modifiedObject, final ValidationConditions<?> validationConditions) {
//        final Class<?> typeClass = validationConditions.getTypeClass();
//        log.debug("Checking immutable_ conditions for " + typeClass.getName());
//        return validateImmutableConditions(originalObject, modifiedObject, validationConditions.getImmutable().keySet(), validationConditions.getImmutable(),
//            typeClass);
        return null;
    }

    private static List<String> validateImmutableConditions(final Object originalObject, final Object modifiedObject, final Set<String> properties,
                                                            final Map<String, ConstraintRefTopGroup> immutableConditions, final Class<?> typeClass) {
        validateArgumentsOrFail(originalObject, properties, immutableConditions, typeClass);
        validateArgumentsOrFail(modifiedObject, properties, immutableConditions, typeClass);

        // TODO better returnType ...
        final List<String> errors = new ArrayList<>();
        for (final String property : properties) {
            final ConstraintRefTopGroup groups = immutableConditions.get(property);
            // Check if all conditions are met. If so, both property values must be equal
            log.debug(">>> Checking immutable_ conditions for property '" + property + "'");
            if (groupsConditionsAreMet(groups, originalObject)) {
                log.debug("Property '" + property + "' IS immutable_");
                final Object originalValue = getPropertyResultObject(property, originalObject);
                final Object modifiedValue = getPropertyResultObject(property, modifiedObject);
                if (!Objects.equals(originalValue, modifiedValue)) {
                    errors.add(defaultImmutableMessage + "." + property);
                }
            } else {
                log.debug("Property '" + property + "' is NOT immutable_ because some conditions are not met.");
            }
        }
        return errors;
    }



    public static <E extends Enum<?>> List<String> validateMandatory(final Object object, final ValidationConditions<?> conditions) {
        return validateMandatory(object, UserPermissions.of(new String[0]), conditions);
    }

    public static <E extends Enum<?>> List<String> validateMandatory(final Object object, final UserPermissions userPermissions,
                                                                   final ValidationConditions<?> conditions) {
        //TODO refactor param validation
        //validateArgumentsOrFail(object, contentProperties, conditions.getContentPermissionMaps().iterator().next(), typeClass);

        final List<Object> userPermissionValues = userPermissions.getValues();
        // TODO better returnType ...
        final List<String> errors = new ArrayList<>();

        for (final String property : conditions.getMandatoryPropertyKeys()) {
            final Map<Permissions, ConstraintRefTopGroup> permissionMap = conditions.getMandatoryPermissionsMap(property);
            final Set<Permissions> constraintPermissions = permissionMap.keySet();
            // Try to get default constraints (w/o any permissions) first
            ConstraintRefTopGroup constraintTopGroup = permissionMap.get(ValidationConditions.NO_PERMISSIONS_NULL_KEY);
            Object matchingPermission = null;
            // If a constraint permission matches any user permission, validate this constraints
            for (final Permissions permissions : constraintPermissions) {
                for (final Object permission : permissions.getValues()) {
                    if (userPermissionValues.contains(permission)) {
                        constraintTopGroup = permissionMap.get(permissions);
                        matchingPermission = permission;
                        break;
                    }
                }
                if (matchingPermission != null) {
                    break;
                }
            }
            if (constraintTopGroup == null) {
                continue; // neither 'default' constraints exist nor constraints for any permissions
            }

            // Check if all conditions are met. If so, the property value must match the contentConstraint
            log.debug(">>> Checking mandatory conditions for property '" + property + "'");
            final String permissionLogPhrase = matchingPermission == null ? "" : " and permission '" + matchingPermission + "'";
            if (groupsConditionsAreMet(constraintTopGroup, object)) {
                final Object value = getPropertyResultObject(property, object);
                log.debug("Property '" + property + "' IS mandatory_");
                if (value == null) {
                    errors.add(defaultMandatoryMessage + "." + property);
                }
            } else {
                log.debug("Property '" + property + "' is NOT mandatory_ because some conditions are not met");
            }
        }
        return errors;
    }


    public static boolean isPropertyImmutable(final String property, final Object object, final ValidationConditions<?> validationConditions) {
        return isPropertyImmutable(property, Collections.emptySet(), object, validationConditions);
    }

    public static <E extends Enum<?>> boolean isPropertyImmutable(final String property, final Set<E> userPermissions, final Object object, final ValidationConditions<?> conditions) {
        log.debug("Checking if property is immutable" + property);

        final Map<Permissions, ConstraintRefTopGroup> conditionPermissions = conditions.getImmutablePermissionsMap(property);
        final Set<Permissions> permissions = conditionPermissions.keySet();
        // Try to get default constraints (w/o any permissions) first
        ConstraintRefTopGroup constraintGroup = conditionPermissions.get(ValidationConditions.NO_PERMISSIONS_NULL_KEY);

        //validateArgumentsOrFail(object, Collections.singleton(property), conditions, typeClass);
        if (constraintGroup != null && groupsConditionsAreMet(constraintGroup, object)) {
            // it is immutable by default, never mind the permissions
            return true;
        }

        // If a constraint permission matches any user permission, validate this constraints
        for (final Permissions permissionBlock : permissions) {
            for (final Object requestedPermission : permissionBlock.getValues()) {
                //noinspection SuspiciousMethodCalls
                if (userPermissions.contains(requestedPermission)) {
                    // found matching permission
                    constraintGroup = conditionPermissions.get(permissionBlock);

                    if (constraintGroup == null || groupsConditionsAreMet(constraintGroup, object)) {
                        //validateArgumentsOrFail(object, Collections.singleton(property), conditions, typeClass);
                        return false;
                    }
                }
            }
        }
        return true;
    }

    public static <E extends Enum<?>> List<String> validateImmutable(final Object originalObject, final Object modifiedObject, final ValidationConditions<?> conditions) {
        return validateImmutable(originalObject, modifiedObject, Collections.emptySet(), conditions);
    }

    public static <E extends Enum<?>> List<String> validateImmutable(final Object originalObject, final Object modifiedObject, final Set<E> userPermissions,
                                                                     final ValidationConditions<?> conditions) {
        //TODO refactor param validation
        //validateArgumentsOrFail(originalObject, contentProperties, conditions.getContentPermissionMaps().iterator().next(), typeClass);
        //validateArgumentsOrFail(modifiedObject, contentProperties, conditions.getContentPermissionMaps().iterator().next(), typeClass);

        final Set<String> userPermissionStrings = userPermissions.stream().map(e -> e.name()).collect(Collectors.toSet());
        // TODO better returnType ...
        final List<String> errors = new ArrayList<>();

        for (final String property : conditions.getImmutablePropertyKeys()) {
            final Map<Permissions, ConstraintRefTopGroup> permissionMap = conditions.getImmutablePermissionsMap(property);
            final Set<Permissions> constraintPermissions = permissionMap.keySet();
            // Try to get default constraints (w/o any permissions) first
            ConstraintRefTopGroup constraintGroup = permissionMap.get(ValidationConditions.NO_PERMISSIONS_NULL_KEY);
            Object matchingPermission = null;
            // If a constraint permission matches any user permission, validate this constraints
            for (final Permissions permissions : constraintPermissions) {
                for (final Object permission : permissions.getValues()) {
                    if (userPermissionStrings.contains(permission)) {
                        constraintGroup = permissionMap.get(permissions);
                        matchingPermission = permission;
                        break;
                    }
                }
                if (matchingPermission != null) {
                    break;
                }
            }
            if (constraintGroup == null) {
                continue; // neither 'default' constraints exist nor constraints for any permissions
            }

            // Check if all conditions are met. If so, both property values must be equal
            log.debug(">>> Checking immutable conditions for property '" + property + "'");
            if (groupsConditionsAreMet(constraintGroup, originalObject)) {
                log.debug("Property '" + property + "' IS immutable_");
                final Object originalValue = getPropertyResultObject(property, originalObject);
                final Object modifiedValue = getPropertyResultObject(property, modifiedObject);
                if (!Objects.equals(originalValue, modifiedValue)) {
                    errors.add(defaultImmutableMessage + "." + property);
                }
            } else {
                log.debug("Property '" + property + "' is NOT immutable because some conditions are not met.");
            }
        }
        return errors;
    }



    public static <E extends Enum<?>> List<String> validateContent(final Object object, final ValidationConditions<?> conditions) {
        return validateContent(object, Collections.emptySet(), conditions);
    }

    public static <E extends Enum<?>> List<String> validateContent(final Object object, final Set<E> userPermissions,
                                                                   final ValidationConditions<?> conditions) {
        //TODO refactor param validation
        //validateArgumentsOrFail(object, contentProperties, conditions.getContentPermissionMaps().iterator().next(), typeClass);

        // TODO better returnType ...
        final List<String> errors = new ArrayList<>();

        for (final String property : conditions.getContentPropertyKeys()) {
            final Map<Permissions, ContentContraintGroup> permissionMap = conditions.getContentPermissionsMap(property);
            final Set<Permissions> permissions = permissionMap.keySet();
            // Try to get default constraints (w/o any permissions) first
            final ContentContraintGroup generalConstraint = permissionMap.get(ValidationConditions.NO_PERMISSIONS_NULL_KEY);
            if (generalConstraint != null) {
                //TODO: is this upside down? are the constrains without permissinos properly checked?
                //Check if all conditions are met. If so, the property value must match the contentConstraint
                final ConstraintRefTopGroup groups = generalConstraint.getConstraintRefTopGroup();
                if (groupsConditionsAreMet(groups, object)) {
                    final Constraint contentConstraint = generalConstraint.getContentConstraint();
                    if (!contraintIsMet(Constraint.ref(property, contentConstraint), object)) {
                        log.debug("Content constraint for permission on property '{}' is NOT fulfilled", property);
                        errors.add(defaultContentMessage + "." + property);
                    } else {
                        log.debug("Content constraint for permission on property '{}' is fulfilled", property);
                    }
                } else {
                    log.debug("Content constraint for permission on property '{}' is NOT validated because some conditions are not met", property);
                }
            }

            // If a constraint permission matches any user permission, validate this constraints
            for (final Permissions permissionBlock : permissions) {
                for (final Object permission : permissionBlock.getValues()) {
                    //noinspection SuspiciousMethodCalls
                    if (userPermissions.contains(permission)) {
                        final ContentContraintGroup constraint = permissionMap.get(permissionBlock);
                        log.debug(">>> Checking content conditions for property '" + property + "'");
                        if (constraint != null) {
                            errors.addAll(checkConstrains(object, property, constraint));
                        }

                    }
                }
            }
        }
        return errors;
    }

    private static List<String> checkConstrains(final Object object, final String property, final ContentContraintGroup contentContraintGroup) {
        final List <String> errors = new ArrayList<>();

        // Check if all conditions are met. If so, the property value must match the contentConstraint
        final ConstraintRefTopGroup groups = contentContraintGroup.getConstraintRefTopGroup();
        if (groupsConditionsAreMet(groups, object)) {
            final Constraint contentConstraint = contentContraintGroup.getContentConstraint();
            if (!contraintIsMet(Constraint.ref(property, contentConstraint), object)) {
                log.debug("Content constraint for permission on property '{}' is NOT fulfilled", property);
                errors.add(defaultContentMessage + "." + property);
            } else {
                log.debug("Content constraint for permission on property '{}' is fulfilled", property);
            }
        } else {
            log.debug("Content constraint for permission on property '{}' is NOT validated because some conditions are not met", property);
        }

        return errors;
    }


    // Does some error checking
    private static void validateArgumentsOrFail(final Object object, final Set<String> properties,
                                                final Map<String, ?> conditions, final Class<?> typeClass) {
        if (object == null || properties == null || conditions == null || conditions.isEmpty()) {
            throw new IllegalArgumentException("Arguments must not be null resp. empty");
        }
        if (!object.getClass().equals(typeClass)) {
            throw new IllegalArgumentException("The object type (" + object.getClass()
                    + " does not equal the type of the ValidationConditionsMap (" + typeClass + ")");
        }
        for (final String property : properties) {
            if (conditions.get(property) == null) {
                throw new IllegalArgumentException("No conditions exist for property " + property);
            }
        }
    }


    public static Object getPropertyResultObject(final String property, final Object object) {
        final String[] propertyParts = property.split("\\.");
        String propertyKey = "";
        Object propertyObject = object;
        Object returnValue = null;
        for (final String propertyPart : propertyParts) {
            propertyKey += "." + propertyPart;
            final PropertyDescriptor cacheKey = new PropertyDescriptor(propertyKey.substring(1), object.getClass());
            final GetterInfo getter = propertyToGetterReturnTypeCache.get(cacheKey);
            try {
                returnValue = getter.getMethod().invoke(propertyObject);
                // If e.g. for "foo.bar" getFoo() returns null, we should prevent a NPE
                // TODO or is it better to throw IllArgEx? Or make this configurable?
                if (returnValue == null) {
                    break;
                }
                final IndexInfo indexInfo = IndexedPropertyHelper.getIndexInfo(propertyKey.substring(1));
                // How to handle [*] etc.?
                /* propertyObject -> List<Object>, bei Simple-Props und Single-Ixd-Props ist size == 1
                 * Sonst loopen. Wenn IndexInfo != LIST or size > 1: propertyObject = new ArrayList<Object>
                 * propertyObject.addAll(returnValues) ...
                 */
                if (indexInfo != null) {
                    if (indexInfo.getIndexType() == IndexedPropertyHelper.IndexType.INCREMENT) {
                        throw new IllegalArgumentException("IndexType.INCREMENT is not yet implemented");
                    }
                    if (indexInfo.getValues().size() != 1) {
                        throw new IllegalArgumentException("IndexType.LIST with more than one value is not yet implemented");
                    }
                    final Integer index = indexInfo.getValues().get(0);
                    if (List.class.isAssignableFrom(returnValue.getClass())) {
                        // Process list
                        if (((List<?>) returnValue).size() > index) {
                            returnValue = ((List<?>) returnValue).get(index);
                            log.debug("list.get(" + index + "): " + returnValue);
                        } else {
                            log.warn("{} does not exist! Returning null. Or better throe an exception? ...", propertyPart);
                            returnValue = null;
                        }
                    } else if (returnValue.getClass().isArray()) {
                        // process array
                        if (Array.getLength(returnValue) > index) {
                            returnValue = Array.get(returnValue, index);
                            log.debug("array[" + index + "]: " + returnValue);
                        } else {
                            log.warn("{} does not exist! Returning null. Or better throe an exception? ...", propertyPart);
                            returnValue = null;
                        }
                    }
                }
            } catch (final IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                throw new RuntimeException(
                        "Exception while invoking method + " + getter.getMethod() + " on " + propertyObject, e);
            }
            if (returnValue == null) {
                return null;
            }
            propertyObject = returnValue;
        }
        return returnValue;
    }

    // If de.swa.easyvalidation.groups are ANDed each group must be met, if they are ORed only one must be met.
    private static boolean groupsConditionsAreMet(final ConstraintRefTopGroup groups, final Object object) {
        if (groups.getConstraintRefGroups().length == 0) {
            log.debug("No constraints defined -> groupsConditionsAreMet = true");
            return true;
        }
        final ConstraintRefTopGroup.Logical groupsOperator = groups.getLogicalOperator();
        for (final ConstraintRefGroup group : groups.getConstraintRefGroups()) {
            if (groupIsMet(group, object)) {
                if (groupsOperator == ConstraintRefTopGroup.Logical.OR) {
                    return true;
                }
            } else {
                if (groupsOperator == ConstraintRefTopGroup.Logical.AND) {
                    return false;
                }
            }
        }
        return (groupsOperator == ConstraintRefTopGroup.Logical.AND) ? true : false;
    }

    // All conditions of an AndGroup must be true, but only one of an OrGroup!
    private static boolean groupIsMet(final ConstraintRefGroup group, final Object object) {
        if (group instanceof AndGroup) {
            for (final ConstraintRef contraint : ((AndGroup) group).getConstraintRefs()) {
                if (!contraintIsMet(contraint, object)) {
                    return false;
                }
            }
            return true;
        } else if (group instanceof OrGroup) {
            for (final ConstraintRef contraint : ((OrGroup) group).getConstraintRefs()) {
                if (contraintIsMet(contraint, object)) {
                    return true;
                }
            }
            return false;
        } else {
            throw new IllegalArgumentException("Wrong ConditionGroup type ...");
        }
    }

    // TODO? handle array resp. list of values for e.g. articled[*].name ...
    private static boolean contraintIsMet(final ConstraintRef contraintRef, final Object object) {
        final Object value = getPropertyResultObject(contraintRef.getProperty(), object);
        log.debug("Value of property '" + contraintRef.getProperty() + "' is '" + value + "'");
//        if (value == null) {
//            return false;
//        }
        return contraintRef.getConstraint().validate(value, object);
    }

    private static Method getGetterMethodOrFail(final String propertyName, final Class<?> clazz) {
        final Map<String, Method> noArgGetters = getNoArgGetterMethodMap(clazz);
        final Method getterMethod = noArgGetters.get(buildGetterName(propertyName));
        if (getterMethod == null) {
            throw new IllegalArgumentException(
                    "No no-arg getter found for property " + propertyName + " in " + clazz.getName());
        }
        return getterMethod;
    }

    private static GetterInfo getGetterReturnType(final Method getterMethod) {
        return new GetterInfo(getterMethod, getterMethod.getReturnType());
    }

    private static Map<String, Method> getNoArgGetterMethodMap(final Class<?> clazz) {
        final BeanInfo beanInfo;
        try {
            beanInfo = Introspector.getBeanInfo(clazz, Object.class);
        } catch (final IntrospectionException e) {
            throw new RuntimeException("Introspection of bean info for " + clazz.getName() + " failed:", e);
        }
        return getNoArgGetterMethodMap(beanInfo);
    }

    private static Map<String, Method> getNoArgGetterMethodMap(final BeanInfo beanInfo) {
        final Map<String, Method> methodNamesMap = new HashMap<>();
        for (final MethodDescriptor md : beanInfo.getMethodDescriptors()) {
            // log.debug("MethodDescriptor: " + md);
            if (md.getName().startsWith("get") && md.getMethod().getParameterTypes().length == 0) {
                methodNamesMap.put(md.getName(), md.getMethod());
                // log.debug("No-arg getter found: " + md);
            }
        }
        return methodNamesMap;
    }

    private static String buildGetterName(final String propertyName) {
        return "get" + propertyName.substring(0, 1).toUpperCase() + propertyName.substring(1);
    }

    public static String getDefaultMandatoryMessage() {
        return defaultMandatoryMessage;
    }

    public static void setDefaultMandatoryMessage(final String message) {
        defaultMandatoryMessage = message;
    }

    public static String getDefaultImmutableMessage() {
        return defaultImmutableMessage;
    }

    public static void setDefaultImmutableMessage(final String message) {
        defaultImmutableMessage = message;
    }

    public static String getDefaultContentMessage() {
        return defaultContentMessage;
    }

    public static void setDefaultContentMessage(final String message) {
        defaultContentMessage = message;
    }

    private static class PropertyDescriptor {
        private final String propertyName;
        private final Class<?> clazz;

        public PropertyDescriptor(final String propertyName, final Class<?> clazz) {
            super();
            this.propertyName = propertyName;
            this.clazz = clazz;
        }

        @Override
        public String toString() {
            return "PropertyDescriptor [propertyName=" + propertyName + ", clazz=" + clazz + "]";
        }

        // java.lang.Class does not implement hashCode() and equals(), but identity hash code is o.k. here, or?
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((clazz == null) ? 0 : clazz.hashCode());
            result = prime * result + ((propertyName == null) ? 0 : propertyName.hashCode());
            return result;
        }

        @Override
        public boolean equals(final Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final PropertyDescriptor other = (PropertyDescriptor) obj;
            if (clazz == null) {
                if (other.clazz != null) {
                    return false;
                }
            } else if (!clazz.equals(other.clazz)) {
                return false;
            }
            if (propertyName == null) {
                if (other.propertyName != null) {
                    return false;
                }
            } else if (!propertyName.equals(other.propertyName)) {
                return false;
            }
            return true;
        }
    }


    static class GetterInfo {
        private final Method method;
        private Class<?> returnType;

        public GetterInfo(final Method method, final Class<?> returnType) {
            super();
            this.method = method;
            this.returnType = returnType;
        }

        public Method getMethod() {
            return method;
        }

        public Class<?> getReturnType() {
            return returnType;
        }

        public void setReturnType(final Class<?> returnType) {
            this.returnType = returnType;
        }

        @Override
        public String toString() {
            return "GetterInfo [method=" + method + ", returnType=" + returnType + "]";
        }
    }
}

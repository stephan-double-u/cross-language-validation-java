package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.constraints.ConstraintRef;
import de.swa.easyvalidation.constraints.Permissions;
import de.swa.easyvalidation.groups.AndGroup;
import de.swa.easyvalidation.groups.ConstraintsSubGroup;
import de.swa.easyvalidation.groups.ConstraintsTopGroup;
import de.swa.easyvalidation.groups.ContentContraints;
import de.swa.easyvalidation.groups.LogicalOperator;
import de.swa.easyvalidation.groups.OrGroup;
import de.swa.easyvalidation.util.IndexedPropertyHelper;
import de.swa.easyvalidation.util.IndexedPropertyHelper.IndexInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.validation.constraints.NotNull;
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

// TODO refactor to 'single instance' ...
public class EasyValidator {
    private static final Logger log = LoggerFactory.getLogger(EasyValidator.class);

    private static final UserPermissions NO_USER_PERMISSIONS = UserPermissions.of(new String[0]);
    private static final Map<PropertyDescriptor, GetterInfo> propertyToGetterReturnTypeCache = new HashMap<>();

    private static String defaultMandatoryMessage = "error.validation.property.mandatory";
    private static String defaultImmutableMessage = "error.validation.property.immutable";
    private static String defaultContentMessage = "error.validation.property.content";


    public static List<String> validateMandatoryConditions(final Object object, final ValidationConditions<?> conditions) {
        return validateMandatoryConditions(object, NO_USER_PERMISSIONS, conditions);
    }

    public static List<String> validateMandatoryConditions(final Object object, final UserPermissions userPermissions,
                                                           final ValidationConditions<?> conditions) {
        Objects.requireNonNull(conditions, "conditions must not be null");

        // TODO better returnType? e.g. ValidationError with key, value?
        final List<String> errors = conditions.getMandatoryPropertyKeys().stream()
                .filter(property -> isPropertyMandatory(property, object, userPermissions, conditions))
                .filter(property -> getPropertyResultObject(property, object) == null)
                .map(property -> defaultMandatoryMessage + "." + property)
                .collect(Collectors.toList());
        return errors;
    }

    public static boolean isPropertyMandatory(final String property, final Object object, final ValidationConditions<?> conditions) {
        return isPropertyMandatory(property, object, NO_USER_PERMISSIONS, conditions);
    }

    public static boolean isPropertyMandatory(final String property, final Object object, final UserPermissions userPermissions,
                                              final ValidationConditions<?> conditions) {
        Objects.requireNonNull(property, "property must not be null");
        Objects.requireNonNull(object, "object must not be null");
        Objects.requireNonNull(userPermissions, "userPermissions must not be null");
        Objects.requireNonNull(conditions, "conditions must not be null");
        validateArguments(property, object, conditions);

        boolean isMandatory = isPropertyMandatoryRespImmutable(conditions.getMandatoryPermissionsMap(property), property, object, userPermissions, conditions);
        log.debug(conditions.getSimpleTypeName() + "." + property + " IS" + (isMandatory ? "" : "NOT") + " mandatory");

        return isMandatory;
    }


    public static <T> List<String> validateImmutableConditions(final Object originalObject, final Object modifiedObject, final ValidationConditions<T> conditions) {
        return validateImmutableConditions(originalObject, modifiedObject, NO_USER_PERMISSIONS, conditions);
    }

    public static <T> List<String> validateImmutableConditions(final Object originalObject, final Object modifiedObject, final UserPermissions userPermissions,
                                                               final ValidationConditions<T> conditions) {
        Objects.requireNonNull(conditions, "conditions must not be null");
        if (originalObject.getClass() != modifiedObject.getClass()) {
            throw new IllegalArgumentException("originalObject and modifiedObject must have same type");
        }

        // TODO better returnType? e.g. ValidationError with key, value?
        final List<String> errors = conditions.getImmutablePropertyKeys().stream()
                .filter(property -> isPropertyImmutable(property, originalObject, userPermissions, conditions))
                .filter(property -> Objects.equals(getPropertyResultObject(property, originalObject), getPropertyResultObject(property, modifiedObject)))
                .map(property -> defaultImmutableMessage + "." + property)
                .collect(Collectors.toList());
        return errors;
    }

    public static boolean isPropertyImmutable(final String property, final Object object, final ValidationConditions<?> conditions) {
        return isPropertyImmutable(property, object, NO_USER_PERMISSIONS, conditions);
    }


    public static boolean isPropertyImmutable(final String property, final Object object, final UserPermissions userPermissions,
                                              final ValidationConditions<?> conditions) {
        Objects.requireNonNull(property, "property must not be null");
        Objects.requireNonNull(object, "object must not be null");
        Objects.requireNonNull(userPermissions, "userPermissions must not be null");
        Objects.requireNonNull(conditions, "conditions must not be null");
        validateArguments(property, object, conditions);

        boolean isImmutable = isPropertyMandatoryRespImmutable(conditions.getImmutablePermissionsMap(property), property, object, userPermissions, conditions);
        log.debug(conditions.getSimpleTypeName() + "." + property + " IS" + (isImmutable ? "" : " NOT") + " immutable");

        return isImmutable;
    }

    private static boolean isPropertyMandatoryRespImmutable(final @NotNull PermissionsMap permissionMap, @NotNull final String property, final @NotNull Object object,
                                                            final @NotNull UserPermissions userPermissions, final @NotNull ValidationConditions<?> conditions) {
        boolean isMet = false;
        final Optional<ConstraintsTopGroup> matchingConstraintTopGroup = getMatchingConstraints(permissionMap, userPermissions);
        if (matchingConstraintTopGroup.isPresent()) {
            isMet = allConditionsAreMet(matchingConstraintTopGroup.get(), object);
        }
        return isMet;
    }

    private static void validateArguments(final @NotNull String property, final @NotNull Object object, final @NotNull ValidationConditions<?> conditions) {
        final Class<?> typeClass = conditions.getTypeClass();
        if (!object.getClass().equals(typeClass)) {
            throw new IllegalArgumentException("The object type (" + object.getClass()
                    + " does not equal the type of the ValidationConditions<" + typeClass + ">");
        }
        EasyValidator.validateProperty(property, typeClass); // TODO? optional here
    }

    private static Optional<ConstraintsTopGroup> getMatchingConstraints(PermissionsMap permissionMap, UserPermissions userPermissions) {
        final Optional<ConstraintsTopGroup> match = permissionMap.entrySet().stream()
                .filter(p -> arePermissionsMatching(p.getKey(), userPermissions))
                .map(p -> p.getValue())
                .findFirst();
        return Optional.of(match.orElseGet(() -> permissionMap.get(ValidationConditions.NO_PERMISSIONS_KEY)));
    }

    private static Optional<ContentContraints> getMatchingContentContraints(ContentPermissionsMap permissionMap, UserPermissions userPermissions) {
        final Optional<ContentContraints> match = permissionMap.entrySet().stream()
                .filter(p -> arePermissionsMatching(p.getKey(), userPermissions))
                .map(p -> p.getValue())
                .findFirst();
        return Optional.of(match.orElseGet(() -> permissionMap.get(ValidationConditions.NO_PERMISSIONS_KEY)));
    }

    private static boolean arePermissionsMatching(Permissions constraintPermissions, UserPermissions userPermissions) {
        // Note: this implements 'match any'
        return constraintPermissions.getValues().stream()
                .filter(p -> userPermissions.getValues().contains(p))
                .findFirst()
                .isPresent();
    }


    public static List<String> validateContentConditions(final Object object, final ValidationConditions<?> conditions) {
        return validateContentConditions(object, NO_USER_PERMISSIONS, conditions);
    }

    public static List<String> validateContentConditions(Object object, UserPermissions userPermissions, ValidationConditions<?> conditions) {
        Objects.requireNonNull(conditions, "conditions must not be null");

        // TODO better returnType? e.g. ValidationError with key, value?
        final List<String> errors = new ArrayList<>();
        for (String property : conditions.getContentPropertyKeys()) {
            final Optional<Constraint> propertyContentConstraint = getPropertyContentConstraint(property, object, userPermissions, conditions);
            if (propertyContentConstraint.isPresent()
            && !constraintIsMet(Constraint.ref(property, propertyContentConstraint.get()), object)) {
                errors.add(defaultContentMessage + "." + property);
            }
        }
        return errors;
    }

    public static Optional<Constraint> getPropertyContentConstraint(final String property, final Object object, final UserPermissions userPermissions,
                                                        final ValidationConditions<?> conditions) {
        Objects.requireNonNull(property, "property must not be null");
        Objects.requireNonNull(object, "object must not be null");
        Objects.requireNonNull(userPermissions, "userPermissions must not be null");
        Objects.requireNonNull(conditions, "conditions must not be null");
        validateArguments(property, object, conditions);

        Constraint contentConstraint = null;
        ContentPermissionsMap permissionMap = conditions.getContentPermissionsMap(property);
        final Optional<ContentContraints> matchingContentConstraints = getMatchingContentContraints(permissionMap, userPermissions);
        if (matchingContentConstraints.isPresent()
            && allConditionsAreMet(matchingContentConstraints.get().getConstraintsTopGroup(), object)) {
            contentConstraint = matchingContentConstraints.get().getContentConstraint();
        }
        log.debug(conditions.getSimpleTypeName() + "." + property + " HAS" + ((contentConstraint != null) ? "" : " NO") + " content conditions");
        return Optional.ofNullable(contentConstraint);
    }



    /**
     * Validates that this property exists for that class.
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


//    private static List<String> checkConstrains(final Object object, final String property, final ContentContraints contentContraints) {
//        final List<String> errors = new ArrayList<>();
//
//        // Check if all conditions are met. If so, the property value must match the contentConstraint
//        final ConstraintsTopGroup groups = contentContraints.getConstraintsTopGroup();
//        if (allConditionsAreMet(groups, object)) {
//            final Constraint contentConstraint = contentContraints.getContentConstraint();
//            if (!constraintIsMet(Constraint.ref(property, contentConstraint), object)) {
//                log.debug("Content constraint for permission on property '{}' is NOT fulfilled", property);
//                errors.add(defaultContentMessage + "." + property);
//            } else {
//                log.debug("Content constraint for permission on property '{}' is fulfilled", property);
//            }
//        } else {
//            log.debug("Content constraint for permission on property '{}' is NOT validated because some conditions are not met", property);
//        }
//
//        return errors;
//    }


    public static Object getPropertyResultObject(final String property, final Object object) {
        if (property == null || object == null) {
            throw new IllegalArgumentException("Arguments must not be null");
        }
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

    // If groups are ANDed each group must be met, if they are ORed only one must be met.
    private static boolean allConditionsAreMet(final ConstraintsTopGroup topGroup, final Object object) {
        if (topGroup.getConstraintsSubGroups().length == 0) {
            log.debug("No constraints defined -> allConditionsAreMet = true");
            return true;
        }
        final LogicalOperator operator = topGroup.getLogicalOperator();
        for (final ConstraintsSubGroup group : topGroup.getConstraintsSubGroups()) {
            if (groupConditionsAreMet(group, object)) {
                if (operator == LogicalOperator.OR) {
                    return true;
                }
            } else {
                if (operator == LogicalOperator.AND) {
                    return false;
                }
            }
        }
        return (operator == LogicalOperator.AND) ? true : false;
    }

    // All conditions of an AndGroup must be true, but only one of an OrGroup!
    private static boolean groupConditionsAreMet(final ConstraintsSubGroup group, final Object object) {
        if (group instanceof AndGroup) {
            for (final ConstraintRef contraint : ((AndGroup) group).getConstraintRefs()) {
                if (!constraintIsMet(contraint, object)) {
                    return false;
                }
            }
            return true;
        } else if (group instanceof OrGroup) {
            for (final ConstraintRef contraint : ((OrGroup) group).getConstraintRefs()) {
                if (constraintIsMet(contraint, object)) {
                    return true;
                }
            }
            return false;
        } else {
            throw new IllegalArgumentException("Wrong ConditionGroup type ...");
        }
    }

    // TODO? handle array resp. list of values for e.g. articled[*].name ...
    private static boolean constraintIsMet(final ConstraintRef constraintRef, final Object object) {
        final Object value = getPropertyResultObject(constraintRef.getProperty(), object);
        log.debug("Value of property '" + constraintRef.getProperty() + "' is '" + value + "'");
//        if (value == null) {
//            return false;
//        }
        return constraintRef.getConstraint().validate(value, object);
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

        // java.lang.Class does not implement hashCode() and equals(), but identity hash code is o.k. here?!
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

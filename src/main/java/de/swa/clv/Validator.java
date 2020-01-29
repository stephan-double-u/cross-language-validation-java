package de.swa.clv;

import de.swa.clv.constraints.Constraint;
import de.swa.clv.constraints.ConstraintRoot;
import de.swa.clv.constraints.PropConstraint;
import de.swa.clv.constraints.Permissions;
import de.swa.clv.groups.*;
import de.swa.clv.groups.RelationsSubGroup;
import de.swa.clv.util.IndexedPropertyHelper;
import de.swa.clv.util.IndexedPropertyHelper.IndexInfo;
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

// TODO refactor to 'single instance' ...
public class Validator {
    private static final Logger log = LoggerFactory.getLogger(Validator.class);

    private static final UserPermissions NO_USER_PERMISSIONS = UserPermissions.of(new String[0]);
    private static final Map<PropertyDescriptor, GetterInfo> propertyToGetterReturnTypeCache = new HashMap<>();

    private static String defaultMandatoryMessage = "error.validation.mandatory";
    private static String defaultImmutableMessage = "error.validation.immutable";
    private static String defaultContentMessage = "error.validation.content";

    private Validator() {
    }

    public static final Validator INSTANCE = new Validator();

    public static Validator instance() {
        return INSTANCE;
    };

    public List<String> validateMandatoryRules(final Object object, final ValidationRules<?> rules) {
        return validateMandatoryRules(object, NO_USER_PERMISSIONS, rules);
    }

    public static List<String> validateMandatoryRules(final Object object, final UserPermissions userPermissions,
                                                           final ValidationRules<?> rules) {
        Objects.requireNonNull(rules, "rules must not be null");

        // TODO better returnType? e.g. ValidationError with key, value?
        final List<String> errors = rules.getMandatoryPropertyKeys().stream()
                .filter(property -> isPropertyMandatory(property, object, userPermissions, rules))
                .filter(property -> getPropertyResultObject(property, object) == null)
                .map(property -> defaultMandatoryMessage + "." + property)
                .collect(Collectors.toList());
        return errors;
    }

    public static boolean isPropertyMandatory(final String property, final Object object, final ValidationRules<?> rules) {
        return isPropertyMandatory(property, object, NO_USER_PERMISSIONS, rules);
    }

    public static boolean isPropertyMandatory(final String property, final Object object, final UserPermissions userPermissions,
                                              final ValidationRules<?> rules) {
        Objects.requireNonNull(property, "property must not be null");
        Objects.requireNonNull(object, "object must not be null");
        Objects.requireNonNull(userPermissions, "userPermissions must not be null");
        Objects.requireNonNull(rules, "rules must not be null");
        validateArguments(property, object, rules);

        boolean isMandatory = isPropertyMandatoryRespImmutable(rules.getMandatoryPermissionsMap(property), property, object, userPermissions, rules);
        log.debug(rules.getSimpleTypeName() + "." + property + " IS" + (isMandatory ? "" : "NOT") + " mandatory");

        return isMandatory;
    }


    public static <T> List<String> validateImmutableRules(final Object originalObject, final Object modifiedObject, final ValidationRules<T> rules) {
        return validateImmutableRules(originalObject, modifiedObject, NO_USER_PERMISSIONS, rules);
    }

    public static <T> List<String> validateImmutableRules(final Object originalObject, final Object modifiedObject, final UserPermissions userPermissions,
                                                               final ValidationRules<T> rules) {
        Objects.requireNonNull(rules, "rules must not be null");
        if (originalObject.getClass() != modifiedObject.getClass()) {
            throw new IllegalArgumentException("originalObject and modifiedObject must have same type");
        }

        // TODO better returnType? e.g. ValidationError with key, value?
        final List<String> errors = rules.getImmutablePropertyKeys().stream()
                .filter(property -> isPropertyImmutable(property, originalObject, userPermissions, rules))
                .filter(property -> !Objects.equals(getPropertyResultObject(property, originalObject), getPropertyResultObject(property, modifiedObject)))
                .map(property -> defaultImmutableMessage + "." + property)
                .collect(Collectors.toList());
        return errors;
    }

    public static boolean isPropertyImmutable(final String property, final Object object, final ValidationRules<?> rules) {
        return isPropertyImmutable(property, object, NO_USER_PERMISSIONS, rules);
    }


    public static boolean isPropertyImmutable(final String property, final Object object, final UserPermissions userPermissions,
                                              final ValidationRules<?> rules) {
        Objects.requireNonNull(property, "property must not be null");
        Objects.requireNonNull(object, "object must not be null");
        Objects.requireNonNull(userPermissions, "userPermissions must not be null");
        Objects.requireNonNull(rules, "rules must not be null");
        validateArguments(property, object, rules);

        boolean isImmutable = isPropertyMandatoryRespImmutable(rules.getImmutablePermissionsMap(property), property, object, userPermissions, rules);
        log.debug(rules.getSimpleTypeName() + "." + property + " IS" + (isImmutable ? "" : " NOT") + " immutable");

        return isImmutable;
    }

    private static boolean isPropertyMandatoryRespImmutable(final PermissionsMap permissionMap, final String property, final Object object,
                                                            final UserPermissions userPermissions, final ValidationRules<?> rules) {
        boolean isMet = false;
        final Optional<RelationsTopGroup> matchingConstraintTopGroup = getMatchingConstraints(permissionMap, userPermissions);
        if (matchingConstraintTopGroup.isPresent()) {
            isMet = allRulesAreMet(matchingConstraintTopGroup.get(), object);
        }
        return isMet;
    }

    private static void validateArguments(final String property, final Object object, final ValidationRules<?> rules) {
        if (property.isEmpty()) {
            throw new IllegalArgumentException("property must not be empty");
        }
        final Class<?> typeClass = rules.getTypeClass();
        if (!object.getClass().equals(typeClass)) {
            throw new IllegalArgumentException("The object type (" + object.getClass()
                    + ") does not equal the type of the " + typeClass);
        }
        Validator.validateProperty(property, typeClass); // TODO? optional here
    }

    private static Optional<RelationsTopGroup> getMatchingConstraints(PermissionsMap permissionMap, UserPermissions userPermissions) {
        final Optional<RelationsTopGroup> match = permissionMap.entrySet().stream()
                .filter(p -> arePermissionsMatching(p.getKey(), userPermissions))
                .map(p -> p.getValue())
                .findFirst();
        return Optional.ofNullable(match.orElseGet(() -> permissionMap.get(ValidationRules.NO_PERMISSIONS_KEY)));
    }

    private static Optional<ContentConstraints> getMatchingContentConstraints(ContentPermissionsMap permissionMap, UserPermissions userPermissions) {
        final Optional<ContentConstraints> match = permissionMap.entrySet().stream()
                .filter(p -> arePermissionsMatching(p.getKey(), userPermissions))
                .map(p -> p.getValue())
                .findFirst();
        return Optional.ofNullable(match.orElseGet(() -> permissionMap.get(ValidationRules.NO_PERMISSIONS_KEY)));
    }

    private static boolean arePermissionsMatching(Permissions constraintPermissions, UserPermissions userPermissions) {
        // Note: this implements 'match any'
        return constraintPermissions.getValues().stream()
                .filter(p -> userPermissions.getValues().contains(p))
                .findFirst()
                .isPresent();
    }


    public static List<String> validateContentRules(final Object object, final ValidationRules<?> rules) {
        return validateContentRules(object, NO_USER_PERMISSIONS, rules);
    }

    public static List<String> validateContentRules(Object object, UserPermissions userPermissions, ValidationRules<?> rules) {
        Objects.requireNonNull(rules, "rules must not be null");

        // TODO better returnType? e.g. ValidationError with key, value?
        final List<String> errors = new ArrayList<>();
        for (String property : rules.getContentPropertyKeys()) {
            final Optional<ConstraintRoot> propertyContentConstraint = getPropertyContentConstraint(property, object, userPermissions, rules);
            ConstraintRoot contentConstraint = propertyContentConstraint.get();
            if (propertyContentConstraint.isPresent()
                    && !constraintIsMet(Constraint.ref(property, contentConstraint), object)) {
                errors.add(defaultContentMessage + "." + rules.getTypeJsonKey() + "." + property + "." + contentConstraint.getType().toLowerCase());
            }
        }
        return errors;
    }

    public static Optional<ConstraintRoot> getPropertyContentConstraint(final String property, final Object object, final UserPermissions userPermissions,
                                                        final ValidationRules<?> rules) {
        Objects.requireNonNull(property, "property must not be null");
        Objects.requireNonNull(object, "object must not be null");
        Objects.requireNonNull(userPermissions, "userPermissions must not be null");
        Objects.requireNonNull(rules, "rules must not be null");
        validateArguments(property, object, rules);

        ConstraintRoot contentConstraint = null;
        ContentPermissionsMap permissionMap = rules.getContentPermissionsMap(property);
        final Optional<ContentConstraints> matchingContentConstraints = getMatchingContentConstraints(permissionMap, userPermissions);
        if (matchingContentConstraints.isPresent()
            && allRulesAreMet(matchingContentConstraints.get().getRelationsTopGroup(), object)) {
            contentConstraint = matchingContentConstraints.get().getContentConstraint();
        }
        log.debug(rules.getSimpleTypeName() + "." + property + " HAS" + ((contentConstraint != null) ? "" : " NO") + " content rules");
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
        Objects.requireNonNull(property, "property must not be null");
        Objects.requireNonNull(property, "clazz must not be null");
        if (property.isEmpty()) {
            throw new IllegalArgumentException("property must not be empty");
        }
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
            if (!IndexedPropertyHelper.getIndexInfo(propertyPart).isPresent()) {
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
            propertyKey += (propertyKey == "" ? "" : ".") + propertyPart;
            propertyToGetterReturnTypeCache.put(new PropertyDescriptor(propertyKey, clazz),
                    getterReturnType);
        }

        return propertyClass;
    }


    public static Object getPropertyResultObject(final String property, final Object object) {
        if (property == null || object == null) {
            throw new IllegalArgumentException("Arguments must not be null");
        }
        final String[] propertyParts = property.split("\\.");
        String propertyKey = "";
        Object propertyObject = object;
        Object returnValue = null;
        for (final String propertyPart : propertyParts) {
            propertyKey += (propertyKey == "" ? "" : ".") + propertyPart;
            final PropertyDescriptor cacheKey = new PropertyDescriptor(propertyKey, object.getClass());
            final GetterInfo getter = propertyToGetterReturnTypeCache.get(cacheKey);
            try {
                returnValue = getter.getMethod().invoke(propertyObject);
                // If e.g. for "foo.bar" getFoo() returns null, we should prevent a NPE
                // TODO or is it better to throw IllArgEx? Or make this configurable?
                // null kann ok sein, wenn es ein 'Blatt' ist: z.B. foo.value, foo.array
                // Fehler wenn foo null ist oder bei foo.array[0], wenn foo.array null ist ...
                if (returnValue == null) {
                    break;
                }
                final Optional<IndexInfo> indexInfoOptional = IndexedPropertyHelper.getIndexInfo(propertyKey);
                // How to handle [*] etc.?
                /* propertyObject -> List<Object>, bei Simple-Props und Single-Ixd-Props ist size == 1
                 * Sonst loopen. Wenn IndexInfo != LIST or size > 1: propertyObject = new ArrayList<Object>
                 * propertyObject.addAll(returnValues) ...
                 */
                if (indexInfoOptional.isPresent()) {
                    final IndexInfo indexInfo = indexInfoOptional.get();
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
                            log.warn("{} does not exist! Returning null. Or better throw an exception? ...", propertyPart);
                            returnValue = null;
                        }
                    } else if (returnValue.getClass().isArray()) {
                        // process array
                        if (Array.getLength(returnValue) > index) {
                            returnValue = Array.get(returnValue, index);
                            //log.debug("array[" + index + "]: " + returnValue);
                        } else {
                            log.warn("{} does not exist! Returning null. Or better throw an exception? ...", propertyPart);
                            returnValue = null;
                        }
                    } else {
                        log.error("Should not happen: Indexed property is neiter a List nor an Array");
                        returnValue = null;
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
    private static boolean allRulesAreMet(final RelationsTopGroup topGroup, final Object object) {
        if (topGroup.getRelationsSubGroups().length == 0) {
            log.debug("No constraints defined -> allRulesAreMet = true");
            return true;
        }
        final LogicalOperator operator = topGroup.getLogicalOperator();
        for (final RelationsSubGroup group : topGroup.getRelationsSubGroups()) {
            if (groupRulesAreMet(group, object)) {
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

    // All rules of an AndGroup must be true, but only one of an OrGroup!
    private static boolean groupRulesAreMet(final RelationsSubGroup group, final Object object) {
        if (group instanceof AndGroup) {
            for (final PropConstraint constraint : ((AndGroup) group).getPropConstraints()) {
                if (!constraintIsMet(constraint, object)) {
                    return false;
                }
            }
            return true;
        } else if (group instanceof OrGroup) {
            for (final PropConstraint constraint : ((OrGroup) group).getPropConstraints()) {
                if (constraintIsMet(constraint, object)) {
                    return true;
                }
            }
            return false;
        } else {
            throw new IllegalArgumentException("Wrong ConditionGroup type ...");
        }
    }

    // TODO? handle array resp. list of values for e.g. articled[*].name ...
    private static boolean constraintIsMet(final PropConstraint propConstraint, final Object object) {
        // TODO ...
        if (IndexedPropertyHelper.isIndexedProperty(propConstraint.getProperty())) {
            List<String> inflatedProperties = inflateIndexedProperty(propConstraint.getProperty(), object);
            // foreach:
            // getPropertyResultObject(inflatedProperty, object);
            // constraintRef.getConstraint().validate(value, object)
        }


        final Object value = getPropertyResultObject(propConstraint.getProperty(), object);
        log.debug("Value of property '" + propConstraint.getProperty() + "' is '" + value + "'");
//        if (value == null) {
//            return false;
//        }
        return propConstraint.getConstraint().validate(value, object);
    }

    // Inflate property with multi-index definitions to properties with single-index definitions, e.g.
    // "foo.bar[0,1].zoo.baz[2-3]" -> ["foo.bar[0].zoo.baz[2]", "foo.bar[0].zoo.baz[3]", "foo.bar[1].zoo.baz[2]", "foo.bar[1].zoo.baz[3]"]
    private static List<String> inflateIndexedProperty(String property, Object object) {
        System.out.println("Inflate " + property);
        final String[] propertyParts = property.split("\\.");
        List<String> inflatedProperties = new ArrayList<>();
        inflatedProperties.add("");
        String delimiter = "";
        for (final String propertyPart : propertyParts) {
            if (IndexedPropertyHelper.isIndexedProperty(propertyPart)) {
                final IndexInfo indexInfo = IndexedPropertyHelper.getIndexInfo(propertyPart).get();
                String propertyPartName = delimiter + propertyPart.substring(0, propertyPart.indexOf('['));
                if (indexInfo.getIndexType() == IndexedPropertyHelper.IndexType.LIST) {
                    inflatedProperties = inflatedProperties.stream()
                            .map(ip -> ip + propertyPartName)
                            .flatMap(p -> inflateListProperty(p, indexInfo.getValues()).stream())
                            .collect(Collectors.toList());
                } else if (indexInfo.getIndexType() == IndexedPropertyHelper.IndexType.INCREMENT) {
                    inflatedProperties = inflatedProperties.stream()
                            .map(ip -> ip + propertyPartName)
                            .flatMap(p -> inflateIncrementProperty(p, object, indexInfo.getValues().get(0), indexInfo.getValues().get(1)).stream())
                            .collect(Collectors.toList());
                } else {
                    throw new IllegalArgumentException("Should never happen! Unknown IndexType: " + indexInfo.getIndexType());
                }
            } else {
                String propertyPartName = delimiter + propertyPart;
                inflatedProperties = inflatedProperties.stream()
                        .map(y -> y + propertyPartName)
                        .collect(Collectors.toList());
            }
            delimiter = ".";
            System.out.println(inflatedProperties);
        }
        inflatedProperties.stream().forEach(p -> validateProperty(p, object.getClass()));
        return inflatedProperties;
    }

    private static List<String> inflateListProperty(String p, List<Integer> indexes) {
        //TODO hier indexe checken?
        return indexes.stream()
                .map(i -> p + "[" + i + "]").
                collect(Collectors.toList());
    }

    private static List<String> inflateIncrementProperty(String property, Object object, Integer startIndex, Integer increment) {
        validateProperty(property, object.getClass());
        final Object propertyResultObject = getPropertyResultObject(property, object);
        int numberOfElements = 0;
        if (List.class.isAssignableFrom(propertyResultObject.getClass())) {
            numberOfElements = ((List<?>) propertyResultObject).size();
        } else if (propertyResultObject.getClass().isArray()) {
            numberOfElements = Array.getLength(propertyResultObject);
        } else {
            throw new IllegalArgumentException("Should not happen! propertyResultObject is neither List nor Array");
        }
        return IntStream.rangeClosed(0, numberOfElements).boxed()
                .map(i -> property + "[" + i + "]")
                .collect(Collectors.toList());
    }

    // try isFoo for booleans, then getFoo for all
    private static Method getGetterMethodOrFail(final String propertyName, final Class<?> clazz) {
        final Map<String, Method> noArgGetters = getNoArgGetterMethodMap(clazz);
        Method getterMethod = noArgGetters.get(buildGetterName("is", propertyName));
        if (getterMethod == null) {
            getterMethod = noArgGetters.get(buildGetterName("get", propertyName));
            if (getterMethod == null) {
                throw new IllegalArgumentException(
                        "No no-arg getter found for property " + propertyName + " in " + clazz.getName());
            }
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
            if (md.getName().startsWith("is") && md.getMethod().getParameterTypes().length == 0
                    && (md.getMethod().getReturnType().equals(boolean.class) || md.getMethod().getReturnType().equals(Boolean.class))) {
                methodNamesMap.put(md.getName(), md.getMethod());
                // log.debug("No-arg getter found: " + md);
            }
            if (md.getName().startsWith("get") && md.getMethod().getParameterTypes().length == 0) {
                methodNamesMap.put(md.getName(), md.getMethod());
                // log.debug("No-arg getter found: " + md);
            }
        }
        return methodNamesMap;
    }

    // 1st char to upper, iff 2nd char is lower!
    private static String buildGetterName(final String prefix, final String propertyName) {
        Character firstCharUpperOrLower = (propertyName.length() > 1 && Character.isLowerCase(propertyName.charAt(1)))
                ? Character.toUpperCase(propertyName.charAt(0)) : propertyName.charAt(0);
        return prefix + firstCharUpperOrLower + propertyName.substring(1);
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

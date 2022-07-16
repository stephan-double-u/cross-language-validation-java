package de.swa.clv;

import de.swa.clv.constraints.*;
import de.swa.clv.groups.*;
import de.swa.clv.util.IndexedPropertyHelper;
import de.swa.clv.util.IndexedPropertyHelper.IndexInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.MethodDescriptor;
import java.lang.reflect.*;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static de.swa.clv.ValidationRules.NO_PERMISSIONS;

public class Validator {

    public static final String ERR_MSG_RULES_NULL = "rules must not be null";
    public static final String ERR_MSG_PROPERTY_NULL = "property must not be null";
    public static final String ERR_MSG_OBJECT_NULL = "object must not be null";
    public static final String ERR_MSG_USER_PERMISSIONS_NULL = "userPermissions must not be null";

    private final Logger log = LoggerFactory.getLogger(Validator.class);

    @SuppressWarnings("squid:S3878")
    private static final UserPermissions NO_USER_PERMISSIONS = UserPermissions.of(new String[0]);
    private final Map<PropertyDescriptor, GetterInfo> propertyToGetterReturnTypeCache = new HashMap<>();

    private String defaultMandatoryMessagePrefix = "error.validation.mandatory.";
    private String defaultImmutableMessagePrefix = "error.validation.immutable.";
    private String defaultContentMessagePrefix = "error.validation.content.";
    private String defaultUpdateMessagePrefix = "error.validation.update.";

    private Validator() {
    }

    private static final Validator INSTANCE = new Validator();

    public static Validator instance() {
        return INSTANCE;
    }


    public List<String> validateMandatoryRules(final Object object, final ValidationRules<?> rules) {
        return validateMandatoryRules(object, NO_USER_PERMISSIONS, rules);
    }

    public List<String> validateMandatoryRules(final Object object, final UserPermissions userPermissions,
            final ValidationRules<?> rules) {
        Objects.requireNonNull(rules, ERR_MSG_RULES_NULL);
        return rules.getMandatoryConditionsKeys().stream()
                .map(property -> validateMandatoryPropertyRules(property, object, userPermissions, rules))
                .filter(Optional::isPresent)
                .map(Optional::get)
                .toList();
    }

    private Optional<String> validateMandatoryPropertyRules(final String property, final Object object,
            final UserPermissions userPermissions, final ValidationRules<?> rules) {
        final Optional<Conditions> conditionsOpt = getMatchingConditions(property, object, userPermissions, rules,
                RulesType.MANDATORY);
        log.debug("{}.{} IS{} mandatory", rules.getSimpleTypeName(), property, (conditionsOpt.isPresent() ? "" : "NOT"));
        if (conditionsOpt.isPresent()
                && !constraintIsMet(Condition.of(property, Equals.notNull()), object)) {
            return Optional.of(buildErrorMessage(defaultMandatoryMessagePrefix, null, rules.getTypeJsonKey(),
                    conditionsOpt.get().getErrorCodeControl(), property));
        }
        return Optional.empty();
    }


    public List<String> validateImmutableRules(final Object originalObject, final Object modifiedObject,
            final ValidationRules<?> rules) {
        return validateImmutableRules(originalObject, modifiedObject, NO_USER_PERMISSIONS, rules);
    }

    public List<String> validateImmutableRules(final Object originalObject, final Object modifiedObject,
            final UserPermissions userPermissions, final ValidationRules<?> rules) {
        Objects.requireNonNull(rules, ERR_MSG_RULES_NULL);
        if (originalObject.getClass() != modifiedObject.getClass()) {
            throw new IllegalArgumentException("originalObject and modifiedObject must have same type");
        }
        return rules.getImmutableConditionsKeys().stream()
                .map(property -> validateImmutablePropertyRules(property, originalObject, modifiedObject,
                        userPermissions, rules))
                .filter(Optional::isPresent)
                .map(Optional::get)
                .toList();
    }

    private Optional<String> validateImmutablePropertyRules(final String property, final Object originalObject,
            final Object modifiedObject, final UserPermissions userPermissions, final ValidationRules<?> rules) {
        final Optional<Conditions> conditionsOpt = getMatchingConditions(property, originalObject, userPermissions,
                rules, RulesType.IMMUTABLE);
        log.debug("{}.{} IS{} immutable", rules.getSimpleTypeName(), property, (conditionsOpt.isPresent() ? "" : "NOT"));
        if (conditionsOpt.isPresent()
                && !propertyValuesEquals(property, originalObject, modifiedObject)) {
            return Optional.of(buildErrorMessage(defaultImmutableMessagePrefix, null, rules.getTypeJsonKey(),
                    conditionsOpt.get().getErrorCodeControl(), property));
        }
        return Optional.empty();
    }


    public List<String> validateContentRules(final Object object, final ValidationRules<?> rules) {
        return validateContentRules(object, NO_USER_PERMISSIONS, rules);
    }

    public List<String> validateContentRules(final Object object, final UserPermissions userPermissions,
            final ValidationRules<?> rules) {
        Objects.requireNonNull(rules, ERR_MSG_RULES_NULL);

        return rules.getContentConditionsKeys().stream()
                .map(property -> validateContentPropertyRules(property, object, userPermissions, rules))
                .filter(Optional::isPresent)
                .map(Optional::get)
                .toList();
    }

    private Optional<String> validateContentPropertyRules(final String property, final Object object,
            final UserPermissions userPermissions, final ValidationRules<?> rules) {
        Optional<Conditions> conditionsOpt = getMatchingConditions(property, object, userPermissions, rules,
                RulesType.CONTENT);
        return validatePropertyRule(property, object, rules.getTypeJsonKey(), conditionsOpt,
                defaultContentMessagePrefix);
    }

    public List<String> validateUpdateRules(final Object originalObject, final Object modifiedObject,
            final ValidationRules<?> rules) {
        return validateUpdateRules(originalObject, modifiedObject, NO_USER_PERMISSIONS, rules);
    }

    public List<String> validateUpdateRules(final Object originalObject, final Object modifiedObject,
            final UserPermissions userPermissions, final ValidationRules<?> rules) {
        Objects.requireNonNull(rules, ERR_MSG_RULES_NULL);
        if (originalObject.getClass() != modifiedObject.getClass()) {
            throw new IllegalArgumentException("originalObject and modifiedObject must have same type");
        }
        return rules.getUpdateConditionsKeys().stream()
                .map(property -> validateUpdatePropertyRules(property, originalObject, modifiedObject, userPermissions,
                        rules))
                .filter(Optional::isPresent)
                .map(Optional::get)
                .toList();
    }

    private Optional<String> validateUpdatePropertyRules(final String property, final Object originalObject,
            final Object modifiedObject, final UserPermissions userPermissions, final ValidationRules<?> rules) {
        Optional<Conditions> conditionsOpt = getMatchingConditions(property, originalObject, userPermissions, rules,
                RulesType.UPDATE);
        return validatePropertyRule(property, modifiedObject, rules.getTypeJsonKey(), conditionsOpt,
                defaultUpdateMessagePrefix);
    }

    private Optional<String> validatePropertyRule(String property, Object object, String typeJsonKey,
            Optional<Conditions> conditionsOpt, String defaultMessagePrefix) {
        if (conditionsOpt.isPresent()) {
            Conditions propConditions = conditionsOpt.get();
            ConstraintRoot constraint = propConditions.getConstraint();
            if (!constraintIsMet(Condition.of(property, constraint), object)) {
                return Optional.of(buildErrorMessage(defaultMessagePrefix, constraint, typeJsonKey,
                        propConditions.getErrorCodeControl(), property));
            }
        }
        return Optional.empty();
    }

    private String buildErrorMessage(String defaultMessagePrefix, ConstraintRoot constraint, String typeJsonKey,
            ErrorCodeControl errorCodeControl, String property) {
        String constraintTypePart = constraint != null ? constraint.getType().toLowerCase() + "." : "";
        String defaultErrorMessage = defaultMessagePrefix + constraintTypePart + typeJsonKey + "." + property;
        return applyErrorCodeControl(errorCodeControl, defaultErrorMessage);
    }

    private String applyErrorCodeControl(ErrorCodeControl errorCodeControl, String defaultErrorMessage) {
        if (errorCodeControl == null) {
            return defaultErrorMessage;
        }
        String code = errorCodeControl.getCode();
        if (errorCodeControl.getType() == UseType.AS_SUFFIX) {
            return defaultErrorMessage + code;
        }
        return code;
    }


    private Optional<Conditions> getMatchingConditions(final String property, final Object object,
            final UserPermissions userPermissions,
            final ValidationRules<?> rules,
            RulesType rulesType) {
        Objects.requireNonNull(property, ERR_MSG_PROPERTY_NULL);
        Objects.requireNonNull(object, ERR_MSG_OBJECT_NULL);
        Objects.requireNonNull(userPermissions, ERR_MSG_USER_PERMISSIONS_NULL);
        Objects.requireNonNull(rules, ERR_MSG_RULES_NULL);
        validateArguments(property, object, rules);

        List<Conditions> conditionsList;
        switch (rulesType) {
        case MANDATORY:
            conditionsList = rules.getMandatoryConditionsList(property);
            break;
        case IMMUTABLE:
            conditionsList = rules.getImmutableConditionsList(property);
            break;
        case CONTENT:
            conditionsList = rules.getContentConditionsList(property);
            break;
        case UPDATE:
            conditionsList = rules.getUpdateConditionsList(property);
            break;
        default:
            throw new IllegalArgumentException("Should not happen - unknown rules type: " + rulesType);
        }
        log.debug("Validate #{} {} rules for {}.{}",
                conditionsList.size(), rulesType, rules.getSimpleTypeName(), property);

        Optional<Conditions> conditions = getMatchingConditions(conditionsList, object, userPermissions);

        log.debug("{}.{} has{} matching {} rule", rules.getSimpleTypeName(), property,
                (conditions.isPresent() ? "" : " NO"), rulesType);
        return conditions;
    }

    private void validateArguments(final String property, final Object object, final ValidationRules<?> rules) {
        if (property.isEmpty()) {
            throw new IllegalArgumentException("property must not be empty");
        }
        final Class<?> typeClass = rules.getTypeClass();
        if (!object.getClass().equals(typeClass)) {
            throw new IllegalArgumentException("The object type (" + object.getClass()
                    + ") does not equal the type of the " + typeClass);
        }
        INSTANCE.validateProperty(property, typeClass); // TODO? optional here
    }

    private Optional<Conditions> getMatchingConditions(List<Conditions> conditionsList, Object object,
            UserPermissions userPermissions) {
        // find first conditions with matching permission and valid reference constraints
        Optional<Conditions> conditions = conditionsList.stream()
                .filter(cc -> arePermissionsMatching(cc.getPermissions(), userPermissions))
                .peek(cc -> log.debug("Checking conditions with matching permissions"))
                .filter(cc -> allConstraintsAreMet(cc.getConditionsTopGroup(), object))
                .findFirst();
        // find first default conditions (w/o any permission) and valid reference constraints
        if (!conditions.isPresent())
            conditions = conditionsList.stream()
                    .filter(cc -> cc.getPermissions() == NO_PERMISSIONS)
                    .peek(cc -> log.debug("Checking conditions without permissions"))
                    .filter(cc -> allConstraintsAreMet(cc.getConditionsTopGroup(), object))
                    .findFirst();
        return conditions;
    }

    private boolean arePermissionsMatching(Permissions constraintPermissions, UserPermissions userPermissions) {
        return constraintPermissions.validate(userPermissions.getValues());
    }

    /**
     * Validates that this property exists for that class.
     *
     * @param property TODO
     * @param clazz TODO
     * @return TODO
     */
    public Class<?> validateProperty(final String property, final Class<?> clazz) {
        Objects.requireNonNull(property, ERR_MSG_PROPERTY_NULL);
        Objects.requireNonNull(clazz, "clazz must not be null");
        if (property.isEmpty()) {
            throw new IllegalArgumentException("property must not be empty");
        }
        final GetterInfo cachedHit = propertyToGetterReturnTypeCache.get(new PropertyDescriptor(property, clazz));
        return cachedHit != null ? cachedHit.getReturnType() : validatePropertyAndCache(property, clazz);
    }

    /**
     * E.g. "location.address.city" for class Article<br/>
     * 1st check: Article.location exist, cache ("location", Article.class) -> (getLocation(), Location.class)<br/>
     * 2nd check: Location.address exist, cache ("location.address", Article.class) -> (getAddress(), Address.class)
     * <br/>
     * 3rd check: Address.city exist, cache ("location.address.city", Article.class) -> (getCity(), String.class)<br/>
     * Supports also indexed properties and aggregate functions, e.g. "foo[*].bar[0/2].zoo#sum"
     */
    private Class<?> validatePropertyAndCache(final String property, final Class<?> propertyClass) {
        // Split a nested property into its parts; e.g. ["location", "address", "city"] and strip off any aggregate
        // function for determining the class of the terminal property part
        final String[] propertyParts = property.split("#")[0].split("\\.");
        Class<?> propertyPartClass = propertyClass;
        String propertyKey = "";
        for (final String propertyPart : propertyParts) {
            GetterInfo getterInfo;
            if (!IndexedPropertyHelper.getIndexInfo(propertyPart).isPresent()) {
                // process simple property
                getterInfo = createGetterInfoForSimpleProperty(propertyPart, propertyPartClass);
            } else {
                // process property with index definitions (e.g. articles[0])
                getterInfo = createGetterInfoForIndexedProperty(propertyPart, propertyPartClass);
            }
            propertyPartClass = getterInfo.getReturnType();
            propertyKey += ("".equals(propertyKey) ? "" : ".") + propertyPart;
            propertyToGetterReturnTypeCache.put(new PropertyDescriptor(propertyKey, propertyClass),
                    getterInfo);
        }
        return propertyPartClass;
    }

    public Optional<AggregateFunction> validateAndGetTerminalAggregateFunctionIfExist(String property) {
        String[] propertySplit = property.split("#");
        if (propertySplit.length > 2) {
            throw new IllegalArgumentException(
                    "Property must not contain more then one aggregate function markers (#): " + property);
        }
        if (propertySplit.length == 2) {
            if (!IndexedPropertyHelper.isIndexedProperty(property)) {
                throw new IllegalArgumentException(
                        "Aggregate functions are only allowed for indexed properties: " + property);
            }
            if (AggregateFunction.sum.name().equals(propertySplit[1])) {
                return Optional.of(AggregateFunction.sum);
            } else if(AggregateFunction.distinct.name().equals(propertySplit[1])) {
                return Optional.of(AggregateFunction.distinct);
            } else {
                throw new IllegalArgumentException(
                        "Property contains unknown aggregate function: " + property);
            }
        }
        return Optional.empty();
    }

    private GetterInfo createGetterInfoForSimpleProperty(String propertyPart, Class<?> propertyPartClass) {
        final Method getterMethod = getGetterMethodOrFail(propertyPart, propertyPartClass);
        return createGetterInfo(getterMethod);
    }

    private GetterInfo createGetterInfoForIndexedProperty(String propertyPart, Class<?> propertyPartClass) {
        final String propertyName = propertyPart.substring(0, propertyPart.indexOf('['));
        final Method getterMethod = getGetterMethodOrFail(propertyName, propertyPartClass);
        GetterInfo getterInfo;
        if (List.class.isAssignableFrom(getterMethod.getReturnType())) {
            // Process list
            // 1. get list field (e.g. articles)
            final Field listField;
            try {
                listField = propertyPartClass.getDeclaredField(propertyName);
            } catch (final NoSuchFieldException | SecurityException e) {
                throw new IllegalArgumentException("Property " + propertyName + " is not a declared field of "
                        + propertyPartClass);
            }
            // 2. get generic type (e.g. Article.class)
            final ParameterizedType listType = (ParameterizedType) listField.getGenericType();
            Type actualTypeArgument = listType.getActualTypeArguments()[0];
            if (actualTypeArgument instanceof WildcardType) {
                WildcardType wildcardType = (WildcardType) actualTypeArgument;
                if (wildcardType.getLowerBounds().length == 0) {
                    actualTypeArgument = wildcardType.getUpperBounds()[0]; // '? extends Foo'
                } else {
                    throw new IllegalArgumentException("Index definitions for generics with lower bounds wildcard " +
                            "type is not implemented (and quite useless(?)): " + propertyPart);
                }
            } else if (actualTypeArgument instanceof TypeVariable) {
                throw new IllegalArgumentException("Index definitions for generics with type variable not " +
                        "implemented yet: " + propertyPart);
            }
            final Class<?> listClass = (Class<?>) actualTypeArgument;
            // 3. get getter method (e.g. getArticles())
            getterInfo = createGetterInfo(getterMethod);
            // 4. ignore return type 'java.util.List' and remember the type of its get(int) call (e.g. Article)
            getterInfo.setReturnType(listClass); // !
        } else if (getterMethod.getReturnType().isArray()) {
            // process array
            final Class<?> arrayTypeClass = getterMethod.getReturnType().getComponentType();
            getterInfo = createGetterInfo(getterMethod);
            //  ignore array type (e.g. Article[]) and remember array component type (e.g. Article)
            getterInfo.setReturnType(arrayTypeClass); // !
        } else {
            throw new IllegalArgumentException("Index definitions are only allowed for properties of type List or " +
                    "arrays: " + propertyPart);
        }
        return getterInfo;
    }

    private GetterInfo createGetterInfo(final Method getterMethod) {
        return new GetterInfo(getterMethod, getterMethod.getReturnType());
    }

    public Object getPropertyResultObject(final String property, final Object object) {
        validateArgumentsNotNullOrFail(property, object);

        String propertyKey = "";
        Object propertyObject = object;
        Object resultObject = null;
        String delimiter = "";

        for (final String propertyPart : property.split("\\.")) {
            propertyKey += delimiter + propertyPart;
            resultObject = invokePropertyGetter(propertyKey, object.getClass(), propertyObject);

            // If e.g. for "foo.bar" getFoo() returns null, we should prevent a NPE
            // null may be ok if it's a leaf: e.g. foo.value, foo.array
            // Error if foo is null or for foo.array[0] if foo.array is null ?
            // TODO or is it better to throw IllArgEx? Or make this configurable?
            if (resultObject == null) {
                break;
            }
            final Optional<IndexInfo> indexInfoOpt = IndexedPropertyHelper.getIndexInfo(propertyPart);
            if (indexInfoOpt.isPresent()) {
                final Integer objectIndex = getSingleIndexOrFail(indexInfoOpt.get());
                resultObject = getIndexedObject(objectIndex, resultObject, propertyKey);
            }
            if (resultObject == null) {
                return null;
            }
            propertyObject = resultObject;
            delimiter = ".";
        }
        return resultObject;
    }

    private Object getIndexedObject(Integer index, Object object, String propertyToLog) {
        Object indexedObject = null;
        if (List.class.isAssignableFrom(object.getClass())) {
            // process list
            if (((List<?>) object).size() > index) {
                indexedObject = ((List<?>) object).get(index);
            } else {
                log.warn("{} does not exist! Returning null. Or better throw an exception? ...", propertyToLog);
            }
        } else if (object.getClass().isArray()) {
            // process array
            if (Array.getLength(object) > index) {
                indexedObject = Array.get(object, index);
            } else {
                log.warn("{} does not exist! Returning null. Or better throw an exception? ...", propertyToLog);
            }
        } else {
            log.error("Should not happen: indexed property is neiter a List nor an Array");
        }
        log.debug("Indexed object {}: {}", propertyToLog, indexedObject);
        return indexedObject;
    }

    private Integer getSingleIndexOrFail(IndexInfo indexInfo) {
        if (indexInfo.getIndexType() != IndexedPropertyHelper.IndexType.LIST || indexInfo.getValues().size() != 1) {
            throw new IllegalArgumentException("Should not happen: multi index definition is not valid here!");
        }
        return indexInfo.getValues().get(0);
    }

    private Object invokePropertyGetter(String propertyKey, Class<?> objectClass, Object propertyObject) {
        final PropertyDescriptor cacheKey = new PropertyDescriptor(propertyKey, objectClass);
        final GetterInfo getter = propertyToGetterReturnTypeCache.get(cacheKey);
        try {
            return getter.getMethod().invoke(propertyObject);
        } catch (IllegalAccessException | InvocationTargetException ex) {
            throw new IllegalStateException(
                    "Exception while invoking method + " + getter.getMethod() + " on " + propertyObject, ex);
        }
    }

    private void validateArgumentsNotNullOrFail(Object... argument) {
        if (argument == null || Arrays.stream(argument).anyMatch(Objects::isNull)) {
            throw new IllegalArgumentException("Arguments must not be null");
        }
    }

    // If groups are ANDed each group must be met, if they are ORed only one must be met.
    private boolean allConstraintsAreMet(final ConditionsTopGroup topGroup, final Object object) {
        if (topGroup.getConstraintsSubGroups().length == 0) {
            log.debug("No constraints defined -> allConstraintsAreMet = true");
            return true;
        }
        final LogicalOperator operator = topGroup.getLogicalOperator();
        for (final ConditionsGroup group : topGroup.getConstraintsSubGroups()) {
            if (groupConstraintsAreMet(group, object)) {
                if (operator == LogicalOperator.OR) {
                    return true;
                }
            } else {
                if (operator == LogicalOperator.AND) {
                    return false;
                }
            }
        }
        return operator == LogicalOperator.AND;
    }

    // All constraints of an AndGroup must be true, but only one of an OrGroup!
    private boolean groupConstraintsAreMet(final ConditionsGroup group, final Object object) {
        if (group instanceof ConditionsAndGroup) {
            for (final PropConstraint constraint : group.getPropConstraints()) {
                if (!constraintIsMet(constraint, object)) {
                    return false;
                }
            }
            return true;
        } else if (group instanceof ConditionsOrGroup) {
            for (final PropConstraint constraint : group.getPropConstraints()) {
                if (constraintIsMet(constraint, object)) {
                    return true;
                }
            }
            return false;
        } else {
            throw new IllegalArgumentException("Should not happen: wrong ConditionGroup type");
        }
    }

    public boolean constraintIsMet(final PropConstraint propConstraint, final Object object) {
        AggregateFunction aggregateFunction = validateAndGetTerminalAggregateFunctionIfExist(
                propConstraint.getProperty()).orElseGet(() -> null);
        String pureProperty = propConstraint.getProperty().split("#")[0];
        List<String> propertiesToCheck = inflatePropertyIfIndexed(pureProperty, object);
        if (aggregateFunction != null) {
            switch (aggregateFunction) {
            case sum -> {
                BigDecimal sum = sumUpPropertyValues(object, propertiesToCheck);
                return propConstraint.getConstraint().validate(sum, object);
            }
            case distinct -> {
                Boolean distinct = distinctCheckForPropertyValues(object, propertiesToCheck);
                return propConstraint.getConstraint().validate(distinct, object);
            }
            default -> throw new IllegalArgumentException("Should not happen. Unsupported: " + aggregateFunction);
            }
        } else {
            for (String propertyToCheck : propertiesToCheck) {
                Object value = getPropertyResultObject(propertyToCheck, object);
                log.debug("Value of property '{}' is '{}'", propConstraint.getProperty(), value);
                if (!propConstraint.getConstraint().validate(value, object)) {
                    return false;
                }
            }
        }
        return true;
    }

    public BigDecimal sumUpPropertyValues(Object object, List<String> propertiesToCheck) {
        BigDecimal sum = BigDecimal.ZERO;
        for (String p : propertiesToCheck) {
            Object propertyResultObject = getPropertyResultObject(p, object);
            sum = sum.add(new BigDecimal(propertyResultObject.toString()));
        }
        return sum;
    }

    public Boolean distinctCheckForPropertyValues(Object object, List<String> propertiesToCheck) {
        Boolean distinct = !propertiesToCheck.isEmpty() ? null : true;
        Object firstElement = null;
        for (String p : propertiesToCheck) {
            Object propertyResultObject = getPropertyResultObject(p, object);
            if (distinct == null) {
                distinct = true;
                firstElement = propertyResultObject;
            } else {
                distinct = !Objects.equals(firstElement, propertyResultObject);
            }
        }
        return distinct;
    }

    public List<String> inflatePropertyIfIndexed(String property, Object object) {
        List<String> propertiesToCheck = new ArrayList<>();
        if (!IndexedPropertyHelper.isIndexedProperty(property)) {
            propertiesToCheck.add(property);
        } else {
            propertiesToCheck = inflateIndexedProperty(property, object);
        }
        return propertiesToCheck;
    }

    private boolean propertyValuesEquals(final String property,
            final Object originalObject,
            final Object modifiedObject) {
        List<String> propertiesToCheck = inflatePropertyIfIndexed(property, originalObject);
        if (propertiesToCheck.size() != inflatePropertyIfIndexed(property, modifiedObject).size()) {
            return false;
        }
        for (String propertyToCheck : propertiesToCheck) {
            Object originalValue = getPropertyResultObject(propertyToCheck, originalObject);
            Object modifiedValue = getPropertyResultObject(propertyToCheck, modifiedObject);
            log.debug("Property '{}': original value is '{}', modified value is '{}'", propertyToCheck, originalValue,
                    modifiedValue);
            if (!Objects.equals(originalValue, modifiedValue)) {
                return false;
            }
        }
        return true;
    }

    // Inflate single property with multi-index definitions to multiple properties with single-index definitions, e.g.
    // "foo.bar[0,1].zoo.baz[2-3]" ->
    // ["foo.bar[0].zoo.baz[2]", "foo.bar[0].zoo.baz[3]", "foo.bar[1].zoo.baz[2]", "foo.bar[1].zoo.baz[3]"]
    protected List<String> inflateIndexedProperty(String property, Object object) {
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
                            .flatMap(ip -> inflateListProperty(ip, indexInfo.getValues()).stream())
                            .collect(Collectors.toList());
                } else if (indexInfo.getIndexType() == IndexedPropertyHelper.IndexType.INCREMENT) {
                    inflatedProperties = inflatedProperties.stream()
                            .map(ip -> ip + propertyPartName)
                            .flatMap(ip -> inflateIncrementProperty(ip, object, indexInfo.getValues().get(0),
                                    indexInfo.getValues().get(1)).stream())
                            .collect(Collectors.toList());
                } else {
                    throw new IllegalArgumentException("Should not happen:  unknown IndexType: "
                            + indexInfo.getIndexType());
                }
            } else {
                String propertyPartName = delimiter + propertyPart;
                inflatedProperties = inflatedProperties.stream()
                        .map(y -> y + propertyPartName)
                        .collect(Collectors.toList());
            }
            delimiter = ".";
            log.debug("Inflated properties for {}: {}", propertyPart, inflatedProperties);
        }
        inflatedProperties.forEach(p -> validateProperty(p, object.getClass()));
        return inflatedProperties;
    }

    // ("foo", List.of(2,4,6)) -> List.of("foo[2]","foo[4]","foo[6]")
    private List<String> inflateListProperty(String p, List<Integer> indexes) {
        return indexes.stream()
                .map(i -> p + "[" + i + "]")
                .toList();
    }

    // ("foo", {"foo":["a","b","c","d","e","f",]}, 1, 2) -> List.of("foo[1]","foo[3]","foo[5]")
    private List<String> inflateIncrementProperty(String property,
            Object object,
            Integer startIndex,
            Integer increment) {
        validateProperty(property, object.getClass());
        final Object propertyResultObject = getPropertyResultObject(property, object);
        if (propertyResultObject == null) {
            return List.of();
        }
        int numberOfElements = 0;
        if (List.class.isAssignableFrom(propertyResultObject.getClass())) {
            numberOfElements = ((List<?>) propertyResultObject).size();
        } else if (propertyResultObject.getClass().isArray()) {
            numberOfElements = Array.getLength(propertyResultObject);
        } else {
            throw new IllegalArgumentException("Should not happen: propertyResultObject is neither List nor Array");
        }
        return IntStream.range(0, numberOfElements).boxed()
                .filter(i -> i >= startIndex)
                .filter(i -> (i - startIndex) % increment == 0)
                .map(i -> property + "[" + i + "]").toList();
    }

    // try isFoo for booleans, then getFoo for all
    private Method getGetterMethodOrFail(final String propertyName, final Class<?> clazz) {
        final Map<String, Method> noArgGetters = getNoArgGetterMethodMap(clazz);
        Method getterMethod = noArgGetters.get(buildGetterName("is", propertyName));
        if (getterMethod == null) {
            getterMethod = noArgGetters.get(buildGetterName("get", propertyName));
            if (getterMethod == null) {
                throw new IllegalArgumentException(
                        "No no-arg getter found for property '" + propertyName + "' in " + clazz.getName());
            }
        }
        return getterMethod;
    }

    private Map<String, Method> getNoArgGetterMethodMap(final Class<?> clazz) {
        final BeanInfo beanInfo;
        try {
            beanInfo = Introspector.getBeanInfo(clazz, Object.class);
        } catch (final IntrospectionException e) {
            throw new IllegalStateException("Introspection of bean info for " + clazz.getName() + " failed:", e);
        }
        return getNoArgGetterMethodMap(beanInfo);
    }

    private Map<String, Method> getNoArgGetterMethodMap(final BeanInfo beanInfo) {
        final Map<String, Method> methodNamesMap = new HashMap<>();
        for (final MethodDescriptor md : beanInfo.getMethodDescriptors()) {
            if (md.getName().startsWith("is") && md.getMethod().getParameterTypes().length == 0
                    && (md.getMethod().getReturnType().equals(boolean.class)
                    || md.getMethod().getReturnType().equals(Boolean.class))) {
                methodNamesMap.put(md.getName(), md.getMethod());
            }
            if (md.getName().startsWith("get") && md.getMethod().getParameterTypes().length == 0) {
                methodNamesMap.put(md.getName(), md.getMethod());
            }
        }
        return methodNamesMap;
    }

    // 1st char to upper, iff 2nd char is lower!
    private String buildGetterName(final String prefix, final String propertyName) {
        char firstCharUpperOrLower = (propertyName.length() > 1 && Character.isLowerCase(propertyName.charAt(1)))
                ? Character.toUpperCase(propertyName.charAt(0)) : propertyName.charAt(0);
        return prefix + firstCharUpperOrLower + propertyName.substring(1);
    }

    public String getDefaultMandatoryMessagePrefix() {
        return defaultMandatoryMessagePrefix;
    }

    public void setDefaultMandatoryMessagePrefix(final String prefix) {
        defaultMandatoryMessagePrefix = prefix;
    }

    public String getDefaultImmutableMessagePrefix() {
        return defaultImmutableMessagePrefix;
    }

    public void setDefaultImmutableMessagePrefix(final String prefix) {
        defaultImmutableMessagePrefix = prefix;
    }

    public String getDefaultContentMessagePrefix() {
        return defaultContentMessagePrefix;
    }

    public void setDefaultContentMessagePrefix(final String prefix) {
        defaultContentMessagePrefix = prefix;
    }

    public String getDefaultUpdateMessagePrefix() {
        return defaultUpdateMessagePrefix;
    }

    public void setDefaultUpdateMessagePrefix(String prefix) {
        this.defaultUpdateMessagePrefix = prefix;
    }

    private record PropertyDescriptor(String propertyName, Class<?> clazz) {
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

    enum RulesType {
        MANDATORY, IMMUTABLE, CONTENT, UPDATE
    }

}

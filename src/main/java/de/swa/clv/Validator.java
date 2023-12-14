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
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static de.swa.clv.ValidationRules.NO_PERMISSIONS;

@SuppressWarnings("squid:S6204")
/**
 *  This class implements the validator part as defined by the
 *  <a href="https://github.com/stephan-double-u/cross-language-validation-schema">Cross Language Validation Schema</a>.
 */
public class Validator {

    private final static Logger log = LoggerFactory.getLogger(Validator.class);

    public static final String ERR_MSG_RULES_NULL = "rules must not be null";
    public static final String ERR_MSG_PROPERTY_NULL = "property must not be null";
    public static final String ERR_MSG_OBJECT_NULL = "object must not be null";
    public static final String ERR_MSG_USER_PERMISSIONS_NULL = "userPermissions must not be null";


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

    public List<String> validateMandatoryRules(final Object editedEntity, final UserPermissions userPermissions,
            final ValidationRules<?> rules) {
        Objects.requireNonNull(rules, ERR_MSG_RULES_NULL);
        return rules.getMandatoryRulesKeys().stream()
                .flatMap(property -> validateMandatoryPropertyRules(property, editedEntity, userPermissions, rules).stream())
                .collect(Collectors.toList());
    }

    private List<String> validateMandatoryPropertyRules(final String property, final Object editedEntity,
            final UserPermissions userPermissions, final ValidationRules<?> rules) {
        List<ValidationRule> matchingRules = getMatchingRules(property, editedEntity, editedEntity, userPermissions,
                rules, RulesType.MANDATORY);
        log.debug("{}.{} IS{} mandatory", rules.getSimpleTypeName(), property, (matchingRules.isEmpty() ? " NOT" : ""));
        return matchingRules.stream()
                .filter(rule -> !conditionIsMet(Condition.of(property, Equals.notNull()), editedEntity, editedEntity))
                .map(rule -> buildErrorMessage(defaultMandatoryMessagePrefix, null, rules.getTypeJsonKey(),
                        rule.getErrorCodeControl(), property)).toList();
    }

    public List<String> validateImmutableRules(final Object currentEntity, final Object editedEntity,
            final ValidationRules<?> rules) {
        return validateImmutableRules(currentEntity, editedEntity, NO_USER_PERMISSIONS, rules);
    }

    public List<String> validateImmutableRules(final Object currentEntity, final Object editedEntity,
            final UserPermissions userPermissions, final ValidationRules<?> rules) {
        Objects.requireNonNull(rules, ERR_MSG_RULES_NULL);
        if (currentEntity.getClass() != editedEntity.getClass()) {
            throw new IllegalArgumentException("currentEntity and editedEntity must have same type");
        }
        return rules.getImmutableRulesKeys().stream()
                .flatMap(property -> validateImmutablePropertyRules(property, currentEntity, editedEntity,
                        userPermissions, rules).stream())
                .collect(Collectors.toList());
    }

    private List<String> validateImmutablePropertyRules(final String property, final Object currentEntity,
            final Object editedEntity, final UserPermissions userPermissions, final ValidationRules<?> rules) {
        List<ValidationRule> matchingRules = getMatchingRules(property, currentEntity, editedEntity, userPermissions,
                rules, RulesType.IMMUTABLE);
        log.debug("{}.{} IS{} immutable", rules.getSimpleTypeName(), property, (matchingRules.isEmpty() ? " NOT" : ""));
        return matchingRules.stream()
                .filter(rule -> !conditionIsMet(Condition.of(property, Value.unchanged()), currentEntity, editedEntity))
                .map(rule -> buildErrorMessage(defaultImmutableMessagePrefix, null, rules.getTypeJsonKey(),
                        rule.getErrorCodeControl(), property)).toList();
    }

    public List<String> validateContentRules(final Object editedEntity, final ValidationRules<?> rules) {
        return validateContentRules(editedEntity, NO_USER_PERMISSIONS, rules);
    }

    public List<String> validateContentRules(final Object editedEntity, final UserPermissions userPermissions,
            final ValidationRules<?> rules) {
        Objects.requireNonNull(rules, ERR_MSG_RULES_NULL);
        return rules.getContentRulesKeys().stream()
                .flatMap(property -> validateContentPropertyRules(property, editedEntity, userPermissions, rules).stream())
                .collect(Collectors.toList());
    }

    private List<String> validateContentPropertyRules(final String property, final Object editedEntity,
            final UserPermissions userPermissions, final ValidationRules<?> rules) {
        List<ValidationRule> matchingRules = getMatchingRules(property, editedEntity, editedEntity, userPermissions,
                rules, RulesType.CONTENT);
        return validatePropertyConstraints(property, editedEntity, editedEntity, rules.getTypeJsonKey(), matchingRules,
                defaultContentMessagePrefix);
    }

    public List<String> validateUpdateRules(final Object currentEntity, final Object editedEntity,
            final ValidationRules<?> rules) {
        return validateUpdateRules(currentEntity, editedEntity, NO_USER_PERMISSIONS, rules);
    }

    public List<String> validateUpdateRules(final Object currentEntity, final Object editedEntity,
            final UserPermissions userPermissions, final ValidationRules<?> rules) {
        Objects.requireNonNull(rules, ERR_MSG_RULES_NULL);
        if (currentEntity.getClass() != editedEntity.getClass()) {
            throw new IllegalArgumentException("currentEntity and editedEntity must have same type");
        }
        return rules.getUpdateRulesKeys().stream()
                .flatMap(property -> validateUpdatePropertyRules(property, currentEntity, editedEntity, userPermissions,
                        rules).stream())
                .collect(Collectors.toList());
    }

    private List<String> validateUpdatePropertyRules(final String property, final Object currentEntity,
            final Object editedEntity, final UserPermissions userPermissions, final ValidationRules<?> rules) {
        List<ValidationRule> matchingRules = getMatchingRules(property, currentEntity, editedEntity, userPermissions,
                rules, RulesType.UPDATE);
        return validatePropertyConstraints(property, editedEntity, currentEntity, rules.getTypeJsonKey(), matchingRules,
                defaultUpdateMessagePrefix);
    }

    private List<String> validatePropertyConstraints(String property, Object thisEntity, Object thatEntity,
            String typeJsonKey, List<ValidationRule> rules, String defaultMessagePrefix) {
        return rules.stream()
                .filter(rule -> !conditionIsMet(Condition.of(property, rule.getConstraint()), thisEntity, thatEntity))
                .map(rule -> buildErrorMessage(defaultMessagePrefix, rule.getConstraint(), typeJsonKey,
                        rule.getErrorCodeControl(), property)).toList();
    }

    private String buildErrorMessage(String defaultMessagePrefix, Constraint constraint, String typeJsonKey,
            ErrorCodeControl errorCodeControl, String property) {
        String constraintTypePart = constraint != null ? constraint.getToken().toLowerCase() + "." : "";
        String defaultErrorMessage = defaultMessagePrefix + constraintTypePart + typeJsonKey + "." + property;
        return applyErrorCodeControl(errorCodeControl, defaultErrorMessage);
    }

    private String applyErrorCodeControl(ErrorCodeControl errorCodeControl, String defaultErrorMessage) {
        if (errorCodeControl == null) {
            return defaultErrorMessage;
        }
        String code = errorCodeControl.code();
        if (errorCodeControl.type() == UseType.AS_SUFFIX) {
            return defaultErrorMessage + code;
        }
        return code;
    }

    private List<ValidationRule> getMatchingRules(final String property, final Object thisEntity,
            Object thatEntity, final UserPermissions userPermissions, final ValidationRules<?> rules,
            RulesType rulesType) {
        Objects.requireNonNull(property, ERR_MSG_PROPERTY_NULL);
        Objects.requireNonNull(thisEntity, ERR_MSG_OBJECT_NULL);
        Objects.requireNonNull(thatEntity, ERR_MSG_OBJECT_NULL);
        Objects.requireNonNull(userPermissions, ERR_MSG_USER_PERMISSIONS_NULL);
        Objects.requireNonNull(rules, ERR_MSG_RULES_NULL);
        validateArguments(property, thisEntity, thatEntity, rules);

        List<ValidationRule> validationRuleList = switch (rulesType) {
            case MANDATORY -> rules.getMandatoryValidationRules(property);
            case IMMUTABLE -> rules.getImmutableValidationRules(property);
            case CONTENT -> rules.getContentValidationRules(property);
            case UPDATE -> rules.getUpdateValidationRules(property);
        };

        log.debug("Validate #{} {} rules for {}.{}",
                validationRuleList.size(), rulesType, rules.getSimpleTypeName(), property);

        List<ValidationRule> matchingRule = getMatchingRules(validationRuleList, thisEntity, thatEntity, userPermissions);

        log.debug("{}.{} has {} matching {} rules", rules.getSimpleTypeName(), property, matchingRule.size(), rulesType);
        return matchingRule;
    }

    private void validateArguments(final String property, final Object thisEntity, final Object thatEntity,
            final ValidationRules<?> rules) {
        if (property.isEmpty()) {
            throw new IllegalArgumentException("Property must not be empty");
        }
        Class<?> thisEntityClass = thisEntity.getClass();
        Class<?> thatEntityClass = thatEntity.getClass();
        Class<?> rulesTypeClass = rules.getTypeClass();
        if (!thisEntityClass.equals(thatEntity.getClass())
                || !thisEntityClass.equals(rulesTypeClass)) {
            throw new IllegalArgumentException("Types must be equal: " + thisEntityClass + " " + thatEntityClass
                    + ", " + rulesTypeClass);
        }
        INSTANCE.validateProperty(property, rulesTypeClass); // TODO? optional here
    }

    // Finds all rules with either no permissions or with matching permissions and with matching conditions
    private List<ValidationRule> getMatchingRules(List<ValidationRule> validationRules, Object thisEntity,
            Object thatEntity, UserPermissions userPermissions) {
        return validationRules.stream()
                .filter(rule -> rule.getPermissions() == NO_PERMISSIONS
                        || rule.getPermissions().validate(userPermissions.getValues()))
                .filter(rule -> allConditionsAreMet(rule.getConditionsTopGroup(), thisEntity, thatEntity))
                .toList();
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
            if (IndexedPropertyHelper.getIndexInfo(propertyPart).isEmpty()) {
                // process simple property
                getterInfo = createGetterInfoForSimpleProperty(propertyPart, propertyPartClass);
            } else {
                // process property with index definitions (e.g. articles[0])
                getterInfo = createGetterInfoForIndexedProperty(propertyPart, propertyPartClass);
            }
            propertyPartClass = getterInfo.getReturnType();
            propertyKey += ("".equals(propertyKey) ? "" : ".") + propertyPart;
            PropertyDescriptor propertyDescriptor = new PropertyDescriptor(propertyKey, propertyClass);
            if (!propertyToGetterReturnTypeCache.containsKey(propertyDescriptor)) {
                log.debug("Cache: {} -> {}", propertyDescriptor, getterInfo);
                propertyToGetterReturnTypeCache.put(propertyDescriptor, getterInfo);
            }
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
            final Field listField = getFieldOrFail(propertyName, propertyPartClass);
            // 2. get generic type (e.g. Article.class)
            final ParameterizedType listType = (ParameterizedType) listField.getGenericType();
            Type actualTypeArgument = listType.getActualTypeArguments()[0];
            if (actualTypeArgument instanceof WildcardType wildcardType) {
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

    private Field getFieldOrFail(String property, Class<?> clazz) {
        return getFieldOrFail(property, clazz, clazz);
    }

    private Field getFieldOrFail(String property, Class<?> startClass, Class<?> superClass) {
        return Arrays.stream(superClass.getDeclaredFields())
                .filter(f -> f.getName().equals(property))
                .findAny()
                .orElseGet(() -> {
                    if (superClass.isInterface() || superClass.getSuperclass() == null) {
                        throw new IllegalArgumentException("Property '" + property + "' is not a declared field of " +
                                "class " + startClass + " (resp. its super classes)");
                    }
                    return getFieldOrFail(property, startClass, superClass.getSuperclass());
                });
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
        if (indexInfo.indexType() != IndexedPropertyHelper.IndexType.LIST || indexInfo.values().size() != 1) {
            throw new IllegalArgumentException("Should not happen: multi index definition is not valid here!");
        }
        return indexInfo.values().get(0);
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
    private boolean allConditionsAreMet(ConditionsTopGroup topGroup, Object thisEntity, Object thatEntity) {
        if (topGroup.equals(ValidationRules.NO_CONDITIONS_TOP_GROUP)) {
            log.debug("No constraints defined -> allConstraintsAreMet = true");
            return true;
        }
        if (topGroup.getConditionsGroups().length == 0) {
            log.debug("No constraints defined -> allConstraintsAreMet = true");
            return true;
        }
        LogicalOperator operator = topGroup.getLogicalOperator();
        for (ConditionsGroup group : topGroup.getConditionsGroups()) {
            if (groupConditionsAreMet(group, thisEntity, thatEntity)) {
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
    private boolean groupConditionsAreMet(ConditionsGroup group, Object thisEntity, Object thatEntity) {
        if (group instanceof ConditionsAndGroup) {
            for (final Condition constraint : group.getConditions()) {
                if (!conditionIsMet(constraint, thisEntity, thatEntity)) {
                    return false;
                }
            }
            return true;
        } else if (group instanceof ConditionsOrGroup) {
            for (final Condition constraint : group.getConditions()) {
                if (conditionIsMet(constraint, thisEntity, thatEntity)) {
                    return true;
                }
            }
            return false;
        } else {
            throw new IllegalArgumentException("Should not happen: wrong ConditionGroup type");
        }
    }

    public boolean conditionIsMet(Condition condition, Object thisEntity, Object thatEntity) {
        String constraintProperty = condition.property();
        AggregateFunction aggregateFunction = validateAndGetTerminalAggregateFunctionIfExist(constraintProperty)
                .orElse(null);
        String pureProperty = constraintProperty.split("#")[0];
        Constraint propertyConstraint = condition.constraint();

        if (propertyConstraint instanceof ValueComparer valueComparer) {
            return validateValueComparerConstraint(pureProperty, aggregateFunction, valueComparer, thisEntity,
                    thatEntity);
        } else {
            return validatePropertyConstraints(pureProperty, aggregateFunction, propertyConstraint, thisEntity,
                    thatEntity);
        }
    }

    /**
     * Evaluates {@code ValueComparer} constraint (i.e. {@code ValueChanged} and {@code ValueUnchanged}) by comparing
     * {@code thisEntity} to {@code thatEntity}.
     * For properties with index definitions (e.g. {@code "amounts[*]"}) the number of elements for both entities must
     * be the same, except for AggregateFunctions:
     * Let be given for example {@code "thisEntity.amounts = [1, 2, 3]"} and {@code "thatEntity.amounts = [4, 2]"}.
     * Then a {@code ValueChanged} constraint for the property {@code "amounts[*]#sum"} is evaluated to {@code true}
     * because the summed values are equal.
     *
     * @param pureProperty the property name without terminal aggregate function suffix
     * @param aggregateFunction nullable AggregateFunction
     * @param valueComparer ValueChanged or ValueUnchanged constraint
     * @param thisEntity the entity that is compared to {@code thatEntity}
     * @param thatEntity the entity that is compared to {@code thisEntity}
     * @return {@code true} if the {@code ValueComparer} constraint is evaluated to {@code true}, otherwise
     * {@code false}
     */
    boolean validateValueComparerConstraint(String pureProperty, AggregateFunction aggregateFunction,
            ValueComparer valueComparer, Object thisEntity, Object thatEntity) {
        List<String> thisPropertiesToCheck = inflatePropertyIfIndexed(pureProperty, thisEntity);
        List<String> thatPropertiesToCheck = inflatePropertyIfIndexed(pureProperty, thatEntity);
        if (aggregateFunction != null) {
            switch (aggregateFunction) {
            case sum -> {
                BigDecimal thisSum = sumUpPropertyValues(thisEntity, thisPropertiesToCheck);
                BigDecimal thatSum = sumUpPropertyValues(thatEntity, thatPropertiesToCheck);
                return valueComparer.validate(thisSum, thatSum);
            }
            case distinct -> {
                Boolean thisDistinct = distinctCheckForPropertyValues(thisEntity, thisPropertiesToCheck);
                Boolean thatDistinct = distinctCheckForPropertyValues(thatEntity, thatPropertiesToCheck);
                return valueComparer.validate(thisDistinct, thatDistinct);
            }
            default -> throw new IllegalArgumentException("Should not happen. Unsupported: " + aggregateFunction);
            }
        }
        if (thisPropertiesToCheck.size() != thatPropertiesToCheck.size()) {
            return valueComparer.getToken().equals(Value.TOKEN.VALUE_CHANGED.name());
        }
        for (String propertyToCheck : thisPropertiesToCheck) {
            Object thisValue = getPropertyResultObject(propertyToCheck, thisEntity);
            Object thatValue = getPropertyResultObject(propertyToCheck, thatEntity);
            log.debug("Property '{}': this value is '{}', that value is '{}'", propertyToCheck, thisValue,
                    thatValue);
            if (!valueComparer.validate(thisValue, thatValue)) {
                return false;
            }
        }
        return true;
    }

    private boolean validatePropertyConstraints(String pureProperty, AggregateFunction aggregateFunction,
            Constraint propertyConstraint, Object thisEntity, Object thatEntity) {
        List<String> propertiesToCheck = inflatePropertyIfIndexed(pureProperty, thisEntity);
        Object targetEntity = thisEntity;
        if (propertyConstraint instanceof ReferenceProperties<?> refConstraint
                && (refConstraint.isOfUpdate() || refConstraint.isOfCurrent())) {
            targetEntity = thatEntity;
        }
        if (aggregateFunction != null) {
            switch (aggregateFunction) {
            case sum -> {
                BigDecimal sum = sumUpPropertyValues(thisEntity, propertiesToCheck);
                return propertyConstraint.validate(sum, targetEntity);
            }
            case distinct -> {
                Boolean distinct = distinctCheckForPropertyValues(thisEntity, propertiesToCheck);
                return propertyConstraint.validate(distinct, targetEntity);
            }
            default -> throw new IllegalArgumentException("Should not happen. Unsupported: " + aggregateFunction);
            }
        } else {
            for (String propertyToCheck : propertiesToCheck) {
                Object value = getPropertyResultObject(propertyToCheck, thisEntity);
                log.debug("Value of property '{}' is '{}'", pureProperty, value);
                if (!propertyConstraint.validate(value, targetEntity)) {
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
        return IndexedPropertyHelper.isIndexedProperty(property)
                ? inflateIndexedProperty(property, object)
                : List.of(property);
    }

    // Inflates single property with multi-index definitions to multiple properties with single-index definitions, e.g.
    // "foo.bar[0,1].zoo.baz[2-3]" ->
    // ["foo.bar[0].zoo.baz[2]", "foo.bar[0].zoo.baz[3]", "foo.bar[1].zoo.baz[2]", "foo.bar[1].zoo.baz[3]"]
    protected List<String> inflateIndexedProperty(String property, Object object) {
        final String[] propertyParts = property.split("\\.");
        List<String> inflatedProperties = new ArrayList<>();
        inflatedProperties.add("");
        String delimiter = "";
        for (final String propertyPart : propertyParts) {
            Optional<IndexInfo> indexInfoOptional = IndexedPropertyHelper.getIndexInfo(propertyPart);
            if (indexInfoOptional.isPresent()) {
                final IndexInfo indexInfo = indexInfoOptional.get();
                String propertyPartName = delimiter + propertyPart.substring(0, propertyPart.indexOf('['));
                if (indexInfo.indexType() == IndexedPropertyHelper.IndexType.LIST) {
                    inflatedProperties = inflatedProperties.stream()
                            .map(ip -> ip + propertyPartName)
                            .flatMap(ip -> inflateListProperty(ip, indexInfo.values()).stream())
                            .collect(Collectors.toList());
                } else if (indexInfo.indexType() == IndexedPropertyHelper.IndexType.INCREMENT) {
                    inflatedProperties = inflatedProperties.stream()
                            .map(ip -> ip + propertyPartName)
                            .flatMap(ip -> inflateIncrementProperty(ip, object, indexInfo.values().get(0),
                                    indexInfo.values().get(1)).stream())
                            .collect(Collectors.toList());
                } else {
                    throw new IllegalArgumentException("Should not happen:  unknown IndexType: "
                            + indexInfo.indexType());
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

    // try isFoo() for booleans, then getFoo() for class, then foo() for record
    private Method getGetterMethodOrFail(final String propertyName, final Class<?> clazz) {
        final Map<String, Method> noArgGetters = getNoArgGetterMethodMap(clazz);
        Method getterMethod = clazz.isRecord()
                ? getGetterMethodForRecord(propertyName, clazz, noArgGetters)
                : getGetterMethodForBean(propertyName, noArgGetters);
        if (getterMethod == null) {
            throw new IllegalArgumentException(
                    "No no-arg getter found for property '" + propertyName + "' in " + clazz.getName());
        }
        return getterMethod;
    }

    private Method getGetterMethodForBean(String propertyName, Map<String, Method> noArgGetters) {
        Method getterMethod = noArgGetters.get(buildGetterName("is", propertyName));
        if (getterMethod == null) {
            getterMethod = noArgGetters.get(buildGetterName("get", propertyName));
        }
        return getterMethod;
    }

    private Method getGetterMethodForRecord(String propertyName, Class<?> clazz, Map<String, Method> noArgGetters) {
        return Arrays.stream(clazz.getRecordComponents())
                .map(RecordComponent::getName)
                .filter(fieldName -> fieldName.equals(propertyName))
                .map(noArgGetters::get)
                .findAny()
                .orElse(null);
    }

    private Map<String, Method> getNoArgGetterMethodMap(final Class<?> clazz) {
        final BeanInfo beanInfo;
        try {
            beanInfo = Introspector.getBeanInfo(clazz, Object.class);
        } catch (final IntrospectionException e) {
            throw new IllegalStateException("Introspection of bean info for " + clazz.getName() + " failed:", e);
        }
        return getNoArgGetterMethodMap(beanInfo, clazz.isRecord());
    }

    private Map<String, Method> getNoArgGetterMethodMap(final BeanInfo beanInfo, boolean isRecord) {
        final Map<String, Method> methodNamesMap = new HashMap<>();
        for (final MethodDescriptor md : beanInfo.getMethodDescriptors()) {
            if (md.getMethod().getParameterTypes().length != 0) {
                continue;
            }
            if (isRecord || md.getName().startsWith("get")
                    || md.getName().startsWith("is") && (md.getMethod().getReturnType().equals(boolean.class)
                    || md.getMethod().getReturnType().equals(Boolean.class))) {
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
        private static final Pattern PATTERN = Pattern.compile("\\[.+?\\]");

        private PropertyDescriptor(String propertyName, Class<?> clazz) {
            // foo[*], foo[1-3] and similar variants should result in the same PropertyDescriptor
            // therefore the index itself is removed
            this.propertyName = PATTERN.matcher(propertyName).replaceAll("[]");
            this.clazz = clazz;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o)
                return true;
            if (!(o instanceof PropertyDescriptor that))
                return false;
            return Objects.equals(propertyName, that.propertyName) && Objects.equals(clazz, that.clazz);
        }

        @Override
        public int hashCode() {
            return Objects.hash(propertyName, clazz);
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

package de.swa.clv;

import de.swa.clv.constraints.*;
import de.swa.clv.groups.ConditionsAndGroup;
import de.swa.clv.groups.ConditionsGroup;
import de.swa.clv.groups.ConditionsOrGroup;
import de.swa.clv.groups.ConditionsTopGroup;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Collectors;

import static de.swa.clv.RulesType.CONTENT;
import static de.swa.clv.RulesType.MANDATORY;
import static de.swa.clv.json.JsonUtil.*;
/**
 * The core public access point to define all types of validation rules as specified by the
 * <a href="https://github.com/stephan-double-u/cross-language-validation-schema">Cross Language Validation Schema</a>.
 * The type parameter {@code T} determines the type for which the rules apply.
 * For each property for which a rule is defined, a no-arg getter method must exist.
 * <br/>Note: <i>the property</i> is to be understood in the broader sense defined by the CLV Schema.
 *
 * @param <T> the type for which the rules are defined
 */
public class ValidationRules<T> {

    private final Logger log = LoggerFactory.getLogger(ValidationRules.class);

    public static final String SCHEMA_VERSION = "0.12";
    public static final Constraint NO_CONSTRAINT = Equals.none("");
    @SuppressWarnings("squid:S3878")
    public static final Permissions NO_PERMISSIONS = Permissions.any(new String[0]);
    public static final ConditionsTopGroup NO_CONDITIONS_TOP_GROUP = ConditionsTopGroup.AND();

    private final PropertyRulesMap mandatoryRulesMap = new PropertyRulesMap();
    private final PropertyRulesMap immutableRulesMap = new PropertyRulesMap();
    private final PropertyRulesMap contentRulesMap = new PropertyRulesMap();
    private final PropertyRulesMap updateRulesMap = new PropertyRulesMap();

    private final Map<RulesType, PropertyRulesMap> rulesTypeMap = Map.of(
            MANDATORY, mandatoryRulesMap,
            RulesType.IMMUTABLE, immutableRulesMap,
            CONTENT, contentRulesMap,
            RulesType.UPDATE, updateRulesMap);

    private final Class<T> typeClass;
    private String typeJsonKey;

    /**
     * Create a {@code ValidationRules} object for validation rules of type {@code T}.
     *
     * @param typeClass the class of type {@code T}
     */
    public ValidationRules(Class<T> typeClass) {
        super();
        this.typeClass = typeClass;
        typeJsonKey = typeClass.getSimpleName().toLowerCase();
    }

    Set<String> getMandatoryRulesKeys() {
        return mandatoryRulesMap.keySet();
    }

    List<ValidationRule> getMandatoryValidationRules(String property) {
        return mandatoryRulesMap.get(property);
    }

    Set<String> getImmutableRulesKeys() {
        return immutableRulesMap.keySet();
    }

    List<ValidationRule> getImmutableValidationRules(String property) {
        return immutableRulesMap.get(property);
    }

    Set<String> getContentRulesKeys() {
        return contentRulesMap.keySet();
    }

    List<ValidationRule> getContentValidationRules(String property) {
        return contentRulesMap.get(property);
    }

    Set<String> getUpdateRulesKeys() {
        return updateRulesMap.keySet();
    }

    List<ValidationRule> getUpdateValidationRules(String property) {
        return updateRulesMap.get(property);
    }

    Class<T> getTypeClass() {
        return typeClass;
    }

    String getSimpleTypeName() {
        return typeClass.getSimpleName();
    }


    /**
     * Defines the property as a mandatory value.
     * <br/>See the note about property names in {@link ValidationRules}.
     *
     * @param property The name of the property this rule is defined for
     * @return the {@link ValidationRule}
     */
    public ValidationRule mandatory(String property) {
        return mandatory(property, NO_PERMISSIONS, NO_CONDITIONS_TOP_GROUP);
    }

    /**
     * Defines the property as a mandatory value, provided the specified condition can be evaluated to {@code true}.
     * <br/>See the note about property names in {@link ValidationRules}.
     *
     * @param property The name of the property this rule is defined for
     * @param condition The {@link Condition} that must be evaluated to {@code true}
     * @return the {@link ValidationRule}
     */
    public ValidationRule mandatory(String property, Condition condition) {
        return mandatory(property, NO_PERMISSIONS, ConditionsGroup.AND(condition));
    }

    /**
     * Defines the property as a mandatory value, provided all conditions of the {@link ConditionsAndGroup} can be
     * evaluated to {@code true}.
     * <br/>See the note about property names in {@link ValidationRules}.
     *
     * @param property The name of the property this rule is defined for
     * @param conditionsAndGroup The {@link ConditionsAndGroup} whose conditions must all be evaluated to {@code true}
     * @return the {@link ValidationRule}
     */
    public ValidationRule mandatory(String property, ConditionsAndGroup conditionsAndGroup) {
        return mandatory(property, NO_PERMISSIONS, ConditionsTopGroup.OR(conditionsAndGroup));
    }

    /**
     * Defines the property as a mandatory value, provided one condition of the {@link ConditionsOrGroup} can be
     * evaluated to {@code true}.
     * <br/>See the note about property names in {@link ValidationRules}.
     *
     * @param property The name of the property this rule is defined for
     * @param conditionsOrGroup The {@link ConditionsOrGroup} of which at least one condition must be {@code true}
     * @return the {@link ValidationRule}
     */
    public ValidationRule mandatory(String property, ConditionsOrGroup conditionsOrGroup) {
        return mandatory(property, NO_PERMISSIONS, ConditionsTopGroup.AND(conditionsOrGroup));
    }

    /**
     * If the conditions under which a property is defined as a mandatory value are so complex that they must be
     * logically linked with both AND and OR, this method may be your last resort.<p/>
     * This method defines the property as a mandatory value, provided the {@link ConditionsTopGroup} can be evaluated
     * to {@code true}.<p/>
     * A {@link ConditionsTopGroup} contains one or more {@link ConditionsAndGroup}s resp.
     * {@link ConditionsOrGroup}s. These groups can be either logically linked with {@code AND} or {@code OR}. I.e. if
     * the logical operator is {@code AND}, the {@link ConditionsTopGroup} is evaluated to {@code true} if all groups
     * can be evaluated to {@code true}. If the logical operator is {@code OR}, the {@link ConditionsTopGroup} is
     * evaluated to {@code true} if at least one group can be evaluated to {@code true}.<br/>
     * Example of a {@link ConditionsTopGroup} where the {@link ConditionsGroup}s are logically linked with AND:
     *  <blockquote><pre>
     *  ConditionsTopGroup.AND(
     *          ConditionsGroup.OR(
     *                  Condition.of("foo", Equals.any(1, 2)),
     *                  Condition.of("bar", Equals.none(3, 4))),
     *          ConditionsGroup.AND(
     *                  Condition.of("zoo", Equals.any(5, 6)),
     *                  Condition.of("baz", Equals.any(7, 8))));
     *  </pre></blockquote>
     * <br/>See the note about property names in {@link ValidationRules}.
     *
     * @param property The name of the property this rule is defined for
     * @param conditionsTopGroup the {@link ConditionsTopGroup} that must be evaluated to {@code true}
     * @return the {@link ValidationRule}
     */
    public ValidationRule mandatory(String property, ConditionsTopGroup conditionsTopGroup) {
        return mandatory(property, NO_PERMISSIONS, conditionsTopGroup);
    }

    /**
     * Defines the property as a mandatory value, provided the specified permissions constraint can be evaluated to
     * {@code true}.
     * <br/>See the note about property names in {@link ValidationRules}.
     *
     * @param property The name of the property this rule is defined for
     * @param permissions the {@link Permissions} constraint that must be evaluated to {@code true}
     * @return the {@link ValidationRule}
     */
    public ValidationRule mandatory(String property, Permissions permissions) {
        return mandatory(property, permissions, NO_CONDITIONS_TOP_GROUP);
    }

    /**
     * Defines the property as a mandatory value, provided the specified permissions constraint can be evaluated to
     * {@code true} and the specified condition can be evaluated to {@code true}.
     * <br/>See the note about property names in {@link ValidationRules}.
     *
     * @param property The name of the property this rule is defined for
     * @param permissions The permissions that must be evaluated to {@code true}
     * @param condition The {@link Condition} that must be evaluated to {@code true}
     * @return the {@link ValidationRule}
     */
    public ValidationRule mandatory(String property, Permissions permissions, Condition condition) {
        return mandatory(property, permissions, ConditionsGroup.AND(condition));
    }

    /**
     * Defines the property as mandatory if permissions match and the {@code ConditionsAndGroup} is true.<p/>
     * I.e. the PropConstraints within each ConditionsAndGroup are ANDed.<p/>
     * E.g. ConditionsGroup.AND(a, b) is evaluated as: a && b
     *
     * @param property       the property name
     * @param conditionsAndGroup the ConditionsAndGroup
     */
    public ValidationRule mandatory(String property, Permissions permissions, ConditionsAndGroup conditionsAndGroup) {
        return mandatory(property, permissions, ConditionsTopGroup.OR(conditionsAndGroup));
    }

    /**
     * Defines the property as mandatory if permissions match and the {@code ConditionsOrGroup} is true.<p/>
     * I.e. the PropConstraints within each ConditionsAndGroup are ORed.<p/>
     * E.g. ConditionsGroup.OR(a, b) is evaluated as: a || b
     *
     * @param property       the property name
     * @param conditionsOrGroup the ConditionsOrGroup
     */
    public ValidationRule mandatory(String property, Permissions permissions, ConditionsOrGroup conditionsOrGroup) {
        return mandatory(property, permissions, ConditionsTopGroup.AND(conditionsOrGroup));
    }

    /**
     * If the logical relation between the conditions are really complex, this method may be your last resort.<p/>
     * This version defines the property as mandatory if permissions match and the {@code ConditionsTopGroup} object
     * evaluates to true.<p/>
     * According to the logical operation the ConditionsAndGroups resp. ConditionsOrGroups are either ANDed or ORed.
     * <p/>
     * E.g. ConditionsTopGroup.AND(ConditionsGroup.OR(a, b), ConditionsGroup.OR(c, d), ConditionsGroup.OR(e, f)) is
     * evaluated as: ...
     * If the logical relation between the constraints are really complicated, this method may be your last resort.
     *
     * @param property       the property name
     * @param permissions the permissions
     * @param topGroup the ConditionsTopGroup
     */
    public ValidationRule mandatory(String property, Permissions permissions, ConditionsTopGroup topGroup) {
        return addPropertyConditions(property, NO_CONSTRAINT, permissions, topGroup, MANDATORY, false);
    }


    public ValidationRule immutable(String property) {
        return immutable(property, NO_PERMISSIONS, NO_CONDITIONS_TOP_GROUP);
    }

    public ValidationRule immutable(String property, Condition condition) {
        return immutable(property, NO_PERMISSIONS, ConditionsGroup.AND(condition));
    }

    public ValidationRule immutable(String property, ConditionsAndGroup conditionsAndGroup) {
        return immutable(property, NO_PERMISSIONS, ConditionsTopGroup.OR(conditionsAndGroup));
    }

    public ValidationRule immutable(String property, ConditionsOrGroup conditionsOrGroup) {
        return immutable(property, NO_PERMISSIONS, ConditionsTopGroup.AND(conditionsOrGroup));
    }

    public ValidationRule immutable(String property, ConditionsTopGroup topGroup) {
        return immutable(property, NO_PERMISSIONS, topGroup);
    }

    public ValidationRule immutable(String property, Permissions permissions) {
        return immutable(property, permissions, NO_CONDITIONS_TOP_GROUP);
    }

    public ValidationRule immutable(String property, Permissions permissions, Condition condition) {
        return immutable(property, permissions, ConditionsGroup.AND(condition));
    }

    public ValidationRule immutable(String property, Permissions permissions, ConditionsAndGroup conditionsAndGroup) {
        return immutable(property, permissions, ConditionsTopGroup.OR(conditionsAndGroup));
    }

    public ValidationRule immutable(String property, Permissions permissions, ConditionsOrGroup conditionsOrGroup) {
        return immutable(property, permissions, ConditionsTopGroup.AND(conditionsOrGroup));
    }

    public ValidationRule immutable(String property, Permissions permissions, ConditionsTopGroup topGroup) {
        return addPropertyConditions(property, NO_CONSTRAINT, permissions, topGroup, RulesType.IMMUTABLE, false);
    }


    /**
     * Defines the content constraint for this property.
     *
     * @param property       the property name
     * @param constraint the constraint
     */
    public <C extends Constraint & IsCreateConstraint> ValidationRule content(String property, C constraint) {
        return content(property, constraint, NO_PERMISSIONS, NO_CONDITIONS_TOP_GROUP);
    }

    /**
     * Defines the content constraint for this property if the {@code PropConstraint} is true.
     *
     * @param property       the property name
     * @param constraint the constraint
     * @param condition the property constraint
     */
    public <C extends Constraint & IsCreateConstraint> ValidationRule content(String property, C constraint,
            Condition condition) {
        return content(property, constraint, NO_PERMISSIONS, ConditionsGroup.AND(condition));
    }

    public <C extends Constraint & IsCreateConstraint> ValidationRule content(String property, C constraint,
            ConditionsAndGroup conditionsAndGroup) {
        return content(property, constraint, NO_PERMISSIONS, ConditionsTopGroup.OR(conditionsAndGroup));
    }

    public <C extends Constraint & IsCreateConstraint> ValidationRule content(String property, C constraint,
            ConditionsOrGroup conditionsOrGroup) {
        return content(property, constraint, NO_PERMISSIONS, ConditionsTopGroup.AND(conditionsOrGroup));
    }

    public <C extends Constraint & IsCreateConstraint> ValidationRule content(String property, C constraint,
            ConditionsTopGroup topGroup) {
        return content(property, constraint, NO_PERMISSIONS, topGroup);
    }

    /**
     * Defines the content constraint for this property if permissions match.
     * @param property       the property name
     * @param constraint the constraint
     * @param permissions the permissions
     */
    public <C extends Constraint & IsCreateConstraint> ValidationRule content(String property, C constraint,
            Permissions permissions) {
        return content(property, constraint, permissions, NO_CONDITIONS_TOP_GROUP);
    }

    /**
     * Defines the content constraint for this property if permissions match and the {@code PropConstraint} is
     * {@code true}.
     *
     * @param property name the property this rule is defined for. According to the schema specification it might be a
     *                 simple, nested or indexed name.
     * @param constraint the constraint that applies to the value of this property.
     * @param permissions permissions that restrict the validity of the rule.
     * @param condition one or more property related conditions that restrict the validity of the rule.
     */
    public <C extends Constraint & IsCreateConstraint> ValidationRule content(String property, C constraint,
            Permissions permissions, Condition condition) {
        return content(property, constraint, permissions, ConditionsGroup.AND(condition));
    }

    public <C extends Constraint & IsCreateConstraint> ValidationRule content(String property, C constraint,
            Permissions permissions, ConditionsAndGroup constraintsAndGroup) {
        return content(property, constraint, permissions, ConditionsTopGroup.OR(constraintsAndGroup));
    }

    public <C extends Constraint & IsCreateConstraint> ValidationRule content(String property, C constraint,
            Permissions permissions, ConditionsOrGroup constraintsOrGroups) {
        return content(property, constraint, permissions, ConditionsTopGroup.AND(constraintsOrGroups));
    }

    public <C extends Constraint & IsCreateConstraint> ValidationRule content(String property, C constraint,
            Permissions permissions, ConditionsTopGroup topGroup) {
        return addPropertyConditions(property, constraint, permissions, topGroup, CONTENT, true);
    }


    public <C extends Constraint & IsUpdateConstraint> ValidationRule update(String property, C constraint) {
        return update(property, constraint, NO_PERMISSIONS, NO_CONDITIONS_TOP_GROUP);
    }

    public <C extends Constraint & IsUpdateConstraint> ValidationRule update(String property, C constraint,
            Condition condition) {
        return update(property, constraint, NO_PERMISSIONS, ConditionsGroup.AND(condition));
    }

    public <C extends Constraint & IsUpdateConstraint> ValidationRule update(String property, C constraint,
            ConditionsAndGroup conditionsAndGroup) {
        return update(property, constraint, NO_PERMISSIONS, ConditionsTopGroup.OR(conditionsAndGroup));
    }

    public <C extends Constraint & IsUpdateConstraint> ValidationRule update(String property, C constraint,
            ConditionsOrGroup conditionsOrGroup) {
        return update(property, constraint, NO_PERMISSIONS, ConditionsTopGroup.AND(conditionsOrGroup));
    }

    public <C extends Constraint & IsUpdateConstraint> ValidationRule update(String property, C constraint,
            ConditionsTopGroup topGroup) {
        return update(property, constraint, NO_PERMISSIONS, topGroup);
    }

    public <C extends Constraint & IsUpdateConstraint> ValidationRule update(String property, C constraint,
            Permissions permissions) {
        return update(property, constraint, permissions, NO_CONDITIONS_TOP_GROUP);
    }

    public <C extends Constraint & IsUpdateConstraint> ValidationRule update(String property, C constraint,
            Permissions permissions, Condition condition) {
        return update(property, constraint, permissions, ConditionsGroup.AND(condition));
    }

    public <C extends Constraint & IsUpdateConstraint> ValidationRule update(String property, C constraint,
            Permissions permissions, ConditionsAndGroup conditionsAndGroup) {
        return update(property, constraint, permissions, ConditionsTopGroup.OR(conditionsAndGroup));
    }

    public <C extends Constraint & IsUpdateConstraint> ValidationRule update(String property, C constraint,
            Permissions permissions, ConditionsOrGroup conditionsOrGroup) {
        return update(property, constraint, permissions, ConditionsTopGroup.AND(conditionsOrGroup));
    }

    public <C extends Constraint & IsUpdateConstraint> ValidationRule update(String property, C constraint,
            Permissions permissions, ConditionsTopGroup topGroup) {
        if (topGroup.equals(NO_CONDITIONS_TOP_GROUP)
                && constraint instanceof ReferenceProperties<?> refConstraint
                && !refConstraint.isOfCurrent()) {
            log.info("The update rule for the property '" + property + "' of type '" + getSimpleTypeName() + "' has " +
                    "a property constraint that references the 'update entity' but has no condition constraint. " +
                    "Therefore, it should better be written as a content rule.");
        }
        return addPropertyConditions(property, constraint, permissions, topGroup, RulesType.UPDATE, true);
    }

    private ValidationRule addPropertyConditions(String property, Constraint constraint,
            Permissions permissions, ConditionsTopGroup conditionsTopGroup, RulesType ruleType,
            boolean isAggregateFunctionAllowed) {
        illegalArgumentExceptionIfNull(property, "property");
        illegalArgumentExceptionIfNull(constraint, "constraint");
        illegalArgumentExceptionIfNull(permissions, "permissions");
        illegalArgumentExceptionIfNull(conditionsTopGroup, "conditionsTopGroup");
        if (property.isEmpty()) {
            throw new IllegalArgumentException("property must not be empty");
        }
        Validator.instance().validateAndGetTerminalAggregateFunctionIfExist(property).ifPresent(f -> {
            if (!isAggregateFunctionAllowed) {
                throw new IllegalArgumentException(
                        "Aggregate functions are not allowed for mandatory and immutable property rules: " + property);
            }
        });
        if (constraint != NO_CONSTRAINT) {
            validateConstraint(property, constraint, ruleType, true);
        }
        validatePropertyAndConditions(property, conditionsTopGroup, ruleType);

        ValidationRule newValidationRule = new ValidationRule(property, constraint, permissions, conditionsTopGroup);
        rulesTypeMap.get(ruleType).getOrInit(property).add(newValidationRule);
        return newValidationRule;
    }

    private void illegalArgumentExceptionIfNull(Object object, String argumentName) {
        if (object == null) {
            throw new IllegalArgumentException(argumentName + "  must not be null");
        }
    }

    private void validatePropertyAndConditions(String property, ConditionsTopGroup topGroup, RulesType ruleType) {
        Validator.instance().validateProperty(property, typeClass);
        for (ConditionsGroup group : topGroup.getConditionsGroups()) {
            for (Condition condition : group.getConditions()) {
                validateConditionConstraint(condition, ruleType);
            }
        }
    }

    private void validateConditionConstraint(Condition condition, RulesType ruleType) {
        if (condition == null) {
            throw new IllegalArgumentException("PropConstraint must not be null");
        }
        validateConstraint(condition.property(), condition.constraint(), ruleType, false);
    }

    private void validateConstraint(String property, Constraint constraint, RulesType ruleType,
            boolean isPropertyConstraint) {
        if (property == null || constraint == null) {
            throw new IllegalArgumentException("Arguments must not be null");
        }
        Class<?> propertyType = Validator.instance().validateProperty(property, typeClass);
        Optional<AggregateFunction> aggregateFunction = Validator.instance()
                .validateAndGetTerminalAggregateFunctionIfExist(property);
        if (aggregateFunction.isPresent() && aggregateFunction.get().equals(AggregateFunction.distinct)) {
            propertyType = Boolean.class;
        }
        // Check that constraint supports propertyType
        if (!constraint.isSupportedType(propertyType)) {
            throw new IllegalArgumentException(
                    "Constraint " + constraint.getClass().getSimpleName() + " does not support type of property "
                            + property + " (" + propertyType + ")");
        }
        // Do further constraint specific validations
        constraint.validateValuesOrFail(typeClass, propertyType);

        if (constraint instanceof ReferenceProperties<?> refConstraint) {
            validateReferencePropertiesContraint(refConstraint, ruleType, isPropertyConstraint);
        }

        if (constraint instanceof ValueComparer valueComparer) {
            validateValueComparerConstraint(valueComparer, ruleType);
        }
    }

    private void validateValueComparerConstraint(ValueComparer valueComparer, RulesType ruleType) {
        if (ruleType == MANDATORY || ruleType == CONTENT) {
            throw new IllegalArgumentException("The constraint " + valueComparer.getClass().getSimpleName() +
                    " is not allowed for rules of type " + ruleType);
        }
    }

    private static void validateReferencePropertiesContraint(ReferenceProperties<?> refConstraint, RulesType ruleType,
            boolean isPropertyConstraint) {
        // check correct usage of ofCurrent() resp. ofUpdate() methods for constraints with property references
        switch (ruleType) {
        case MANDATORY, CONTENT -> {
            if (refConstraint.isOfUpdate() || refConstraint.isOfCurrent()) {
                throw new IllegalArgumentException("The method calls ofUpdate() resp. ofCurrent() are not " +
                        "allowed for rules of type " + ruleType);
            }
        }
        case IMMUTABLE, UPDATE -> {
            if (isPropertyConstraint && refConstraint.isOfUpdate()) {
                throw new IllegalArgumentException("A property constraint that references properties is " +
                        "always evaluated against the 'update' instance anyway. Using ofUpdate() is " +
                        "therefore considered as invalid here.");
            } else if (!isPropertyConstraint && refConstraint.isOfCurrent()) {
                throw new IllegalArgumentException("A condition constraint that references properties is " +
                        "always evaluated against the 'current' instance anyway. Using ofCurrent() is " +
                        "therefore considered as invalid here.");
            }
        }
        default -> throw new IllegalArgumentException("Should not happen!");
        }
    }

    /**
     * Overwrites the default key that is used as an identifier for the generic type {@code T} when the validation
     * rules are serialized to JSON. The default key is the lowercase simple name of the class {@code T}.
     *
     * @param typeJsonKey the json key for the type that should be use for serialization
     */
    public void setTypeJsonKey(String typeJsonKey) {
        this.typeJsonKey = typeJsonKey;
    }

    public String getTypeJsonKey() {
        return typeJsonKey;
    }

    public String serializeToJson() {
        return serializeToJson(this);
    }

    public static String serializeToJson(ValidationRules<?>... rules) {
        List<ValidationRules<?>> validationRulesList = Arrays.asList(rules);
        String jsonAll = asKey("schemaVersion") + quoted(SCHEMA_VERSION) + ",";
        jsonAll += asKey("mandatoryRules") + asObject(validationRulesList.stream()
                .map(ValidationRules::serializeMandatoryRules)
                .filter(json -> !json.isEmpty())
                .collect(Collectors.joining(","))) + ",";
        jsonAll += asKey("immutableRules") + asObject(validationRulesList.stream()
                .map(ValidationRules::serializeImmutableRules)
                .filter(json -> !json.isEmpty())
                .collect(Collectors.joining(","))) + ",";
        jsonAll += asKey("contentRules") + asObject(validationRulesList.stream()
                .map(ValidationRules::serializeContentRules)
                .filter(json -> !json.isEmpty())
                .collect(Collectors.joining(","))) + ",";
        jsonAll += asKey("updateRules") + asObject(validationRulesList.stream()
                .map(ValidationRules::serializeUpdateRules)
                .filter(json -> !json.isEmpty())
                .collect(Collectors.joining(",")));
        return asObject(jsonAll);
    }

    private String serializeMandatoryRules() {
        String mapJson = mandatoryRulesMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson) : "";
    }

    private String serializeImmutableRules() {
        String mapJson = immutableRulesMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson): "";
    }

    private String serializeContentRules() {
        String mapJson = contentRulesMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson): "";
    }

    private String serializeUpdateRules() {
        String mapJson = updateRulesMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson): "";
    }

}


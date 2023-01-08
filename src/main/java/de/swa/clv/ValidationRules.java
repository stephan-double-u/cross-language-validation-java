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
 * A class to define property validation rules for (possibly nested) properties of type {@code T}.
 * For each property a no-arg getter method must exist.
 *
 * @param <T> the type for which the rules are defined
 */
public class ValidationRules<T> {

    private final Logger log = LoggerFactory.getLogger(ValidationRules.class);

    public static final String SCHEMA_VERSION = "0.10";
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

    public ValidationRules(Class<T> typeClass) {
        super();
        this.typeClass = typeClass;
        typeJsonKey = typeClass.getSimpleName().toLowerCase();
    }

    public Set<String> getMandatoryRulesKeys() {
        return mandatoryRulesMap.keySet();
    }

    public List<ValidationRule> getMandatoryValidationRules(String property) {
        return mandatoryRulesMap.get(property);
    }

    public Set<String> getImmutableRulesKeys() {
        return immutableRulesMap.keySet();
    }

    public List<ValidationRule> getImmutableValidationRules(String property) {
        return immutableRulesMap.get(property);
    }

    public Set<String> getContentRulesKeys() {
        return contentRulesMap.keySet();
    }

    public List<ValidationRule> getContentValidationRules(String property) {
        return contentRulesMap.get(property);
    }

    public Set<String> getUpdateRulesKeys() {
        return updateRulesMap.keySet();
    }

    public List<ValidationRule> getUpdateValidationRules(String property) {
        return updateRulesMap.get(property);
    }

    public Class<T> getTypeClass() {
        return typeClass;
    }

    public String getSimpleTypeName() {
        return typeClass.getSimpleName();
    }


    /**
     * Defines the property as mandatory.
     *
     * @param property the property name
     */
    public ValidationRule mandatory(String property) {
        return mandatory(property, NO_PERMISSIONS, NO_CONDITIONS_TOP_GROUP);
    }

    /**
     * Defines the property as mandatory if the {@code PropConstraint} is true.
     *
     * @param property the property name
     * @param conditionConstraint the property constraint
     */
    public ValidationRule mandatory(String property, ConditionConstraint conditionConstraint) {
        return mandatory(property, NO_PERMISSIONS, ConditionsGroup.AND(conditionConstraint));
    }

    /**
     * Defines the property as mandatory if the {@code ConditionsAndGroup} is true.<p/>
     * I.e. the PropConstraints within each ConditionsAndGroup are ANDed.<p/>
     * E.g. ConditionsGroup.AND(a, b) is evaluated as: a && b
     *
     * @param property           the property name
     * @param conditionsAndGroup the ConditionsAndGroup
     */
    public ValidationRule mandatory(String property, ConditionsAndGroup conditionsAndGroup) {
        return mandatory(property, NO_PERMISSIONS, ConditionsTopGroup.OR(conditionsAndGroup));
    }

    /**
     * Defines the property as mandatory if the {@code ConditionsOrGroup} is true.<p/>
     * I.e. the PropConstraints within each ConditionsAndGroup are ORed.<p/>
     * E.g. ConditionsGroup.OR(a, b) is evaluated as: a || b
     *
     * @param property       the property name
     * @param conditionsOrGroup the ConditionsOrGroup
     */
    public ValidationRule mandatory(String property, ConditionsOrGroup conditionsOrGroup) {
        return mandatory(property, NO_PERMISSIONS, ConditionsTopGroup.AND(conditionsOrGroup));
    }

    /**
     * If the logical relation between the conditions are really complex, this method may be your last resort.<p/>
     * This version defines the property as mandatory if the {@code ConditionsTopGroup} object evaluates to true.<p/>
     * According to the logical operation the ConditionsAndGroups resp. ConditionsOrGroups are either ANDed or ORed.
     * <p/> E.g. ConditionsTopGroup.AND(ConditionsGroup.OR(a, b), ConditionsGroup.OR(c, d), ConditionsGroup.OR(e, f)]
     * is evaluated as: TODO
     *
     * @param property       the property name
     * @param topGroup the ConditionsTopGroup
     */
    public ValidationRule mandatory(String property, ConditionsTopGroup topGroup) {
        return mandatory(property, NO_PERMISSIONS, topGroup);
    }

    /**
     * Defines the property as mandatory if permissions match.
     *
     * @param property    the property name
     * @param permissions the permissions
     */
    public ValidationRule mandatory(String property, Permissions permissions) {
        return mandatory(property, permissions, NO_CONDITIONS_TOP_GROUP);
    }

    /**
     * Defines the property as mandatory if permissions match and the {@code PropConstraint} is true.
     *
     * @param property       the property name
     * @param conditionConstraint the property constraint
     */
    public ValidationRule mandatory(String property, Permissions permissions, ConditionConstraint conditionConstraint) {
        return mandatory(property, permissions, ConditionsGroup.AND(conditionConstraint));
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

    public ValidationRule immutable(String property, ConditionConstraint conditionConstraint) {
        return immutable(property, NO_PERMISSIONS, ConditionsGroup.AND(conditionConstraint));
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

    public ValidationRule immutable(String property, Permissions permissions, ConditionConstraint conditionConstraint) {
        return immutable(property, permissions, ConditionsGroup.AND(conditionConstraint));
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
     * @param conditionConstraint the property constraint
     */
    public <C extends Constraint & IsCreateConstraint> ValidationRule content(String property, C constraint,
            ConditionConstraint conditionConstraint) {
        return content(property, constraint, NO_PERMISSIONS, ConditionsGroup.AND(conditionConstraint));
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
     * @param conditionConstraint one or more property related conditions that restrict the validity of the rule.
     */
    public <C extends Constraint & IsCreateConstraint> ValidationRule content(String property, C constraint,
            Permissions permissions, ConditionConstraint conditionConstraint) {
        return content(property, constraint, permissions, ConditionsGroup.AND(conditionConstraint));
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
            ConditionConstraint conditionConstraint) {
        return update(property, constraint, NO_PERMISSIONS, ConditionsGroup.AND(conditionConstraint));
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
            Permissions permissions, ConditionConstraint conditionConstraint) {
        return update(property, constraint, permissions, ConditionsGroup.AND(conditionConstraint));
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

    public ValidationRule addPropertyConditions(String property, Constraint constraint,
            Permissions permissions, ConditionsTopGroup topGroup, RulesType ruleType,
            boolean isAggregateFunctionAllowed) {
        Objects.requireNonNull(property, "property must not be null");
        Objects.requireNonNull(constraint, "constraint must not be null");
        Objects.requireNonNull(permissions, "permissions must not be null");
        Objects.requireNonNull(topGroup, "topGroup must not be null");
        Objects.requireNonNull(ruleType, "topGroup must not be null");
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
        validatePropertyAndConditions(property, topGroup, ruleType);

        ValidationRule newValidationRule = new ValidationRule(property, constraint, permissions, topGroup);
        rulesTypeMap.get(ruleType).getOrInit(property).add(newValidationRule);
        return newValidationRule;
    }

    private void validatePropertyAndConditions(String property, ConditionsTopGroup topGroup, RulesType ruleType) {
        Validator.instance().validateProperty(property, typeClass);
        for (ConditionsGroup group : topGroup.getConditionsGroups()) {
            for (ConditionConstraint conditionConstraint : group.getConstraints()) {
                validateConditionConstraint(conditionConstraint, ruleType);
            }
        }
    }

    private void validateConditionConstraint(ConditionConstraint conditionConstraint, RulesType ruleType) {
        if (conditionConstraint == null) {
            throw new IllegalArgumentException("PropConstraint must not be null");
        }
        validateConstraint(conditionConstraint.property(), conditionConstraint.constraint(), ruleType, false);
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


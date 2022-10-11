package de.swa.clv;

import de.swa.clv.constraints.*;
import de.swa.clv.groups.ConditionsAndGroup;
import de.swa.clv.groups.ConditionsGroup;
import de.swa.clv.groups.ConditionsOrGroup;
import de.swa.clv.groups.ConditionsTopGroup;

import java.util.*;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.*;

/**
 * A class to define property validation rules for (possibly nested) properties of type {@code T}.
 * For each property a no-arg getter method must exist.
 *
 * @param <T> the type for which the rules are defined
 */
public class ValidationRules<T> {

    public static final String SCHEMA_VERSION = "0.8";
    public static final Constraint NO_CONSTRAINT = Equals.none("");
    @SuppressWarnings("squid:S3878")
    public static final Permissions NO_PERMISSIONS = Permissions.any(new String[0]);
    public static final ConditionsTopGroup NO_CONDITIONS_TOP_GROUP = ConditionsTopGroup.AND();

    private final PropertyRulesMap mandatoryRulesMap = new PropertyRulesMap();
    private final PropertyRulesMap immutableRulesMap = new PropertyRulesMap();
    private final PropertyRulesMap contentRulesMap = new PropertyRulesMap();
    private final PropertyRulesMap updateRulesMap = new PropertyRulesMap();

    private final Class<T> typeClass;
    private String typeJsonKey;

    public ValidationRules(final Class<T> typeClass) {
        super();
        this.typeClass = typeClass;
        typeJsonKey = typeClass.getSimpleName().toLowerCase();
    }

    public Set<String> getMandatoryRulesKeys() {
        return mandatoryRulesMap.keySet();
    }

    public List<ValidationRule> getMandatoryValidationRules(final String property) {
        return mandatoryRulesMap.get(property);
    }

    public Set<String> getImmutableRulesKeys() {
        return immutableRulesMap.keySet();
    }

    public List<ValidationRule> getImmutableValidationRules(final String property) {
        return immutableRulesMap.get(property);
    }

    public Set<String> getContentRulesKeys() {
        return contentRulesMap.keySet();
    }

    public List<ValidationRule> getContentValidationRules(final String property) {
        return contentRulesMap.get(property);
    }

    public Set<String> getUpdateRulesKeys() {
        return updateRulesMap.keySet();
    }

    public List<ValidationRule> getUpdateValidationRules(final String property) {
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
    public ValidationRule mandatory(final String property) {
        return mandatory(property, NO_PERMISSIONS, NO_CONDITIONS_TOP_GROUP);
    }

    /**
     * Defines the property as mandatory if the {@code PropConstraint} is true.
     *
     * @param property the property name
     * @param propConstraint the property constraint
     */
    public ValidationRule mandatory(final String property, final PropConstraint propConstraint) {
        return mandatory(property, NO_PERMISSIONS, ConditionsGroup.AND(propConstraint));
    }

    /**
     * Defines the property as mandatory if the {@code ConditionsAndGroup} is true.<p/>
     * I.e. the PropConstraints within each ConditionsAndGroup are ANDed.<p/>
     * E.g. ConditionsGroup.AND(a, b) is evaluated as: a && b
     *
     * @param property           the property name
     * @param conditionsAndGroup the ConditionsAndGroup
     */
    public ValidationRule mandatory(final String property, final ConditionsAndGroup conditionsAndGroup) {
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
    public ValidationRule mandatory(final String property, final ConditionsOrGroup conditionsOrGroup) {
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
    public ValidationRule mandatory(final String property, final ConditionsTopGroup topGroup) {
        return mandatory(property, NO_PERMISSIONS, topGroup);
    }

    /**
     * Defines the property as mandatory if permissions match.
     *
     * @param property    the property name
     * @param permissions the permissions
     */
    public ValidationRule mandatory(final String property, final Permissions permissions) {
        return mandatory(property, permissions, NO_CONDITIONS_TOP_GROUP);
    }

    /**
     * Defines the property as mandatory if permissions match and the {@code PropConstraint} is true.
     *
     * @param property       the property name
     * @param propConstraint the property constraint
     */
    public ValidationRule mandatory(final String property, final Permissions permissions,
            final PropConstraint propConstraint) {
        return mandatory(property, permissions, ConditionsGroup.AND(propConstraint));
    }

    /**
     * Defines the property as mandatory if permissions match and the {@code ConditionsAndGroup} is true.<p/>
     * I.e. the PropConstraints within each ConditionsAndGroup are ANDed.<p/>
     * E.g. ConditionsGroup.AND(a, b) is evaluated as: a && b
     *
     * @param property       the property name
     * @param conditionsAndGroup the ConditionsAndGroup
     */
    public ValidationRule mandatory(final String property, final Permissions permissions,
            final ConditionsAndGroup conditionsAndGroup) {
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
    public ValidationRule mandatory(final String property, final Permissions permissions,
            final ConditionsOrGroup conditionsOrGroup) {
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
    public ValidationRule mandatory(final String property, final Permissions permissions, final ConditionsTopGroup topGroup) {
        return addPropertyConditions(property, NO_CONSTRAINT, permissions, topGroup,
                mandatoryRulesMap.getOrInit(property), false);
    }


    public ValidationRule immutable(final String property) {
        return immutable(property, NO_PERMISSIONS, NO_CONDITIONS_TOP_GROUP);
    }

    public ValidationRule immutable(final String property, final PropConstraint propConstraint) {
        return immutable(property, NO_PERMISSIONS, ConditionsGroup.AND(propConstraint));
    }

    public ValidationRule immutable(final String property, final ConditionsAndGroup conditionsAndGroup) {
        return immutable(property, NO_PERMISSIONS, ConditionsTopGroup.OR(conditionsAndGroup));
    }

    public ValidationRule immutable(final String property, final ConditionsOrGroup conditionsOrGroup) {
        return immutable(property, NO_PERMISSIONS, ConditionsTopGroup.AND(conditionsOrGroup));
    }

    public ValidationRule immutable(final String property, final ConditionsTopGroup topGroup) {
        return immutable(property, NO_PERMISSIONS, topGroup);
    }

    public ValidationRule immutable(final String property, final Permissions permissions) {
        return immutable(property, permissions, NO_CONDITIONS_TOP_GROUP);
    }

    public ValidationRule immutable(final String property, final Permissions permissions,
            final PropConstraint propConstraint) {
        return immutable(property, permissions, ConditionsGroup.AND(propConstraint));
    }

    public ValidationRule immutable(final String property, final Permissions permissions,
            final ConditionsAndGroup conditionsAndGroup) {
        return immutable(property, permissions, ConditionsTopGroup.OR(conditionsAndGroup));
    }

    public ValidationRule immutable(final String property, final Permissions permissions,
            final ConditionsOrGroup conditionsOrGroup) {
        return immutable(property, permissions, ConditionsTopGroup.AND(conditionsOrGroup));
    }

    public ValidationRule immutable(final String property, final Permissions permissions,
            final ConditionsTopGroup topGroup) {
        return addPropertyConditions(property, NO_CONSTRAINT, permissions, topGroup,
                immutableRulesMap.getOrInit(property), false);
    }


    /**
     * Defines the content constraint for this property.
     *
     * @param property       the property name
     * @param constraint the constraint
     */
    public ValidationRule content(final String property, final Constraint constraint) {
        return content(property, constraint, NO_PERMISSIONS, NO_CONDITIONS_TOP_GROUP);
    }

    /**
     * Defines the content constraint for this property if the {@code PropConstraint} is true.
     *
     * @param property       the property name
     * @param constraint the constraint
     * @param propConstraint the property constraint
     */
    public ValidationRule content(final String property, final Constraint constraint,
            final PropConstraint propConstraint) {
        return content(property, constraint, NO_PERMISSIONS, ConditionsGroup.AND(propConstraint));
    }

    public ValidationRule content(final String property, final Constraint constraint,
            final ConditionsAndGroup conditionsAndGroup) {
        return content(property, constraint, NO_PERMISSIONS, ConditionsTopGroup.OR(conditionsAndGroup));
    }

    public ValidationRule content(final String property, final Constraint constraint,
            final ConditionsOrGroup conditionsOrGroup) {
        return content(property, constraint, NO_PERMISSIONS, ConditionsTopGroup.AND(conditionsOrGroup));
    }

    public ValidationRule content(final String property, final Constraint constraint,
            final ConditionsTopGroup topGroup) {
        return content(property, constraint, NO_PERMISSIONS, topGroup);
    }

    /**
     * Defines the content constraint for this property if permissions match.
     * @param property       the property name
     * @param constraint the constraint
     * @param permissions the permissions
     */
    public ValidationRule content(final String property, final Constraint constraint, final Permissions permissions) {
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
     * @param propConstraint one or more property related conditions that restrict the validity of the rule.
     */
    public ValidationRule content(final String property, final Constraint constraint, final Permissions permissions,
                        final PropConstraint propConstraint) {
        return content(property, constraint, permissions, ConditionsGroup.AND(propConstraint));
    }

    public ValidationRule content(final String property, final Constraint constraint, final Permissions permissions,
                        final ConditionsAndGroup constraintsAndGroup) {
        return content(property, constraint, permissions, ConditionsTopGroup.OR(constraintsAndGroup));
    }

    public ValidationRule content(final String property, final Constraint constraint, final Permissions permissions,
                        final ConditionsOrGroup constraintsOrGroups) {
        return content(property, constraint, permissions, ConditionsTopGroup.AND(constraintsOrGroups));
    }

    public ValidationRule content(final String property, final Constraint constraint, final Permissions permissions,
                        final ConditionsTopGroup topGroup) {
        return addPropertyConditions(property, constraint, permissions, topGroup,
                contentRulesMap.getOrInit(property), true);
    }


    public ValidationRule update(final String property, final Constraint constraint,
            final PropConstraint propConstraint) {
        return update(property, constraint, NO_PERMISSIONS, ConditionsGroup.AND(propConstraint));
    }

    public ValidationRule update(final String property, final Constraint constraint,
                       final ConditionsAndGroup conditionsAndGroup) {
        return update(property, constraint, NO_PERMISSIONS, ConditionsTopGroup.OR(conditionsAndGroup));
    }

    public ValidationRule update(final String property, final Constraint constraint,
                       final ConditionsOrGroup conditionsOrGroup) {
        return update(property, constraint, NO_PERMISSIONS, ConditionsTopGroup.AND(conditionsOrGroup));
    }

    public ValidationRule update(final String property, final Constraint constraint, final ConditionsTopGroup topGroup) {
        return update(property, constraint, NO_PERMISSIONS, topGroup);
    }

    public ValidationRule update(final String property, final Constraint constraint, final Permissions permissions,
            final PropConstraint propConstraint) {
        return update(property, constraint, permissions, ConditionsGroup.AND(propConstraint));
    }

    public ValidationRule update(final String property, final Constraint constraint, final Permissions permissions,
                       final ConditionsAndGroup conditionsAndGroup) {
        return update(property, constraint, permissions, ConditionsTopGroup.OR(conditionsAndGroup));
    }

    public ValidationRule update(final String property, final Constraint constraint, final Permissions permissions,
                       final ConditionsOrGroup conditionsOrGroup) {
        return update(property, constraint, permissions, ConditionsTopGroup.AND(conditionsOrGroup));
    }

    public ValidationRule update(final String property, final Constraint constraint, final Permissions permissions,
            final ConditionsTopGroup topGroup) {
        return addPropertyConditions(property, constraint, permissions, topGroup,
                updateRulesMap.getOrInit(property), true);
    }

    public ValidationRule addPropertyConditions(final String property, final Constraint constraint,
            final Permissions permissions, final ConditionsTopGroup topGroup,
            List<ValidationRule> validationRuleList,
            boolean isAggregateFunctionAllowed) {
        Objects.requireNonNull(property, "property must not be null");
        Objects.requireNonNull(constraint, "constraint must not be null");
        Objects.requireNonNull(permissions, "permissions must not be null");
        Objects.requireNonNull(topGroup, "topGroup must not be null");
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
            validateConstraint(property, constraint);
        }
        validatePropertyAndConditions(property, topGroup);
        ValidationRule newValidationRule = new ValidationRule(property, constraint, permissions, topGroup);
        validationRuleList.add(newValidationRule);
        return newValidationRule;
    }

    //TODO remove permanently if decision is made that permission validation makes no sense
    private void validatePermissions(Set<Permissions> permissionsMap, Permissions permissions, String propertyToLog) {
        permissions.validateValuesOrFail(null, null);
        // Check if any permissions are 'not unique';
        // e.g. Perm.any(A,B), followed by Perm.any(C,B) -> constraints for B is not unique
        if (permissions == NO_PERMISSIONS) {
            if (permissionsMap.contains(permissions)) {
                throw new IllegalArgumentException(String.format("Validation rules for property '%s' " +
                        "(w/o permissions) are already defined.", propertyToLog));
            }
        } else {
            if(permissions.getValues().isEmpty()) {
                throw new IllegalArgumentException(String.format("Permissions for property '%s' must not be empty.",
                        propertyToLog));
            }
            Optional<String> nonUniquePermission = checkPermissionUniqueness(permissionsMap, permissions);
            if (nonUniquePermission.isPresent()) {
                throw new IllegalArgumentException(String.format("Validation rules for property '%s' and permission " +
                                "'%s' are already defined.",
                        propertyToLog, nonUniquePermission.get()));
            }
        }
    }

    private Optional<String> checkPermissionUniqueness(Set<Permissions> existingPermissions,
                                                       Permissions newPermissions) {
        // Flatten all permission values
        final Set<String> existingPerms = existingPermissions.stream()
                .map(Constraint::getValues)
                .flatMap(Collection::stream)
                .map(Object::toString)
                .collect(Collectors.toSet());
        // Search for permission that is already defined
        final Set<String> newPermissionsAsSet = newPermissions.getValues().stream()
                .map(Object::toString)
                .collect(Collectors.toSet());
        existingPerms.retainAll(newPermissionsAsSet);
        return existingPerms.stream().findFirst();
    }

    private void validatePropertyAndConditions(String property, ConditionsTopGroup topGroup) {
        Validator.instance().validateProperty(property, typeClass);
        for (final ConditionsGroup group : topGroup.getConstraintsSubGroups()) {
            for (final PropConstraint propConstraint : group.getPropConstraints()) {
                validatePropConstraint(propConstraint);
            }
        }
    }

    private void validatePropConstraint(final PropConstraint propConstraint) {
        if (propConstraint == null) {
            throw new IllegalArgumentException("PropConstraint must not be null");
        }
        validateConstraint(propConstraint.property(), propConstraint.constraint());
    }

    private void validateConstraint(final String property, final Constraint constraint) {
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
    }

    /**
     * Overwrites the default key that is used as an identifier for the generic type {@code T} when the validation
     * rules are serialized to JSON. The default key is the lowercase simple name of the class {@code T}.
     *
     * @param typeJsonKey the json key for the type that should be use for serialization
     */
    public void setTypeJsonKey(final String typeJsonKey) {
        this.typeJsonKey = typeJsonKey;
    }

    public String getTypeJsonKey() {
        return typeJsonKey;
    }

    public String serializeToJson() {
        return serializeToJson(this);
    }

    public static String serializeToJson(final ValidationRules<?>... rules) {
        final List<ValidationRules<?>> validationRulesList = Arrays.asList(rules);
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
        final String mapJson = mandatoryRulesMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson) : "";
    }

    private String serializeImmutableRules() {
        final String mapJson = immutableRulesMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson): "";
    }

    private String serializeContentRules() {
        final String mapJson = contentRulesMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson): "";
    }

    private String serializeUpdateRules() {
        final String mapJson = updateRulesMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson): "";
    }

}


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
 * A class to combine property validation rules for (possibly nested) properties of type {@code T},
 * providing some comfortable put methods, e.g. when more than one rule is needed. For each property a no-arg
 * getter method must exist.
 *
 * @param <T> the type for which the rules are defined
 */
public class ValidationRules<T> {

    public static final ConstraintRoot NO_CONSTRAINT = Equals.none("");
    @SuppressWarnings("squid:S3878")
    public static final Permissions NO_PERMISSIONS = Permissions.any(new String[0]);
    public static final ConditionsTopGroup NO_CONDITIONS_TOP_GROUP = ConditionsTopGroup.AND();

    private final PropertyConditionsMap mandatoryConditionsMap = new PropertyConditionsMap();
    private final PropertyConditionsMap immutableConditionsMap = new PropertyConditionsMap();
    private final PropertyConditionsMap contentConditionsMap = new PropertyConditionsMap();
    private final PropertyConditionsMap updateConditionsMap = new PropertyConditionsMap();

    private final Class<T> typeClass;
    private String typeJsonKey;

    public ValidationRules(final Class<T> typeClass) {
        super();
        this.typeClass = typeClass;
        typeJsonKey = typeClass.getSimpleName().toLowerCase();
    }

    public Set<String> getMandatoryConditionsKeys() {
        return mandatoryConditionsMap.keySet();
    }

    public List<Conditions> getMandatoryConditionsList(final String property) {
        return mandatoryConditionsMap.get(property);
    }

    public Set<String> getImmutableConditionsKeys() {
        return immutableConditionsMap.keySet();
    }

    public List<Conditions> getImmutableConditionsList(final String property) {
        return immutableConditionsMap.get(property);
    }

    public Set<String> getContentConditionsKeys() {
        return contentConditionsMap.keySet();
    }

    public List<Conditions> getContentConditionsList(final String property) {
        return contentConditionsMap.get(property);
    }

    public Set<String> getUpdateConditionsKeys() {
        return updateConditionsMap.keySet();
    }

    public List<Conditions> getUpdateConditionsList(final String property) {
        return updateConditionsMap.get(property);
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
    public void mandatory(final String property) {
        mandatory(property, NO_PERMISSIONS);
    }

    /**
     * Defines the property as mandatory if permissions match.
     *
     * @param property    the property name
     * @param permissions the permissions
     */
    public void mandatory(final String property, final Permissions permissions) {
        mandatory(property, permissions, NO_CONDITIONS_TOP_GROUP);
    }

    /**
     * Defines the property as mandatory if all {@code PropConstraint}s are true.
     * I.e. the PropConstraints are ANDed. Convenience method for mandatory(String, ConditionsAndGroup).
     *
     * @param property       the property name
     * @param propConstraints
     */
    public void mandatory(final String property, final PropConstraint... propConstraints) {
        mandatory(property, NO_PERMISSIONS, propConstraints);
    }

    /**
     * Defines the property as mandatory if permissions match and all {@code PropConstraint}s are true.
     * I.e. the PropConstraint are ANDed. Convenience method for mandatory(String, Permissions, ConditionsAndGroup).
     *
     * @param property       the property name
     * @param permissions    the permissions
     * @param propConstraints
     */
    public void mandatory(final String property, final Permissions permissions,
                          final PropConstraint... propConstraints) {
        mandatory(property, permissions, ConditionsTopGroup.AND(ConditionsGroup.AND(propConstraints)));
    }

    /**
     * Defines the property as mandatory if at least one of the {@code ConditionsAndGroup}s is true.<p/>
     * I.e. the ConditionsAndGroup are ORed where the PropConstraints within each ConditionsAndGroup are ANDed.<p/>
     * E.g. [ConditionsGroup.AND(a, b), ConditionsGroup.AND(c, d)] is evaluated as: a && b || c && d
     *
     * @param property
     * @param conditionsAndGroups
     */
    public void mandatory(final String property, final ConditionsAndGroup... conditionsAndGroups) {
        mandatory(property, NO_PERMISSIONS, conditionsAndGroups);
    }

    /**
     * Defines the property as mandatory if permissions match and at least one of the {@code ConditionsAndGroups}s is
     * true.<p/>
     * I.e. the ConditionsAndGroup are ORed where the PropConstraints within each ConditionsAndGroup are ANDed.<p/>
     * E.g. [ConditionsGroup.AND(a, b), ConditionsGroup.AND(c, d)] is evaluated as: a && b || c && d
     *
     * @param property            the property name
     * @param permissions         the permissions
     * @param conditionsAndGroups
     */
    public void mandatory(final String property, final Permissions permissions,
                          final ConditionsAndGroup... conditionsAndGroups) {
        mandatory(property, permissions, ConditionsTopGroup.OR(conditionsAndGroups));
    }

    /**
     * Defines the property as mandatory if all of the {@code ConditionsOrGroup}s are true.<p/>
     * I.e. the ConditionsOrGroup are ANDed where the PropConstraints within each ConditionsOrGroup are ORed.<p/>
     * E.g. [ConditionsGroup.OR(e, f), ConditionsGroup.OR(g, h)] is evaluated as: (e || f) && (g || h)
     *
     * @param property    the property name
     * @param constraintsOrGroups
     */
    public void mandatory(final String property, final ConditionsOrGroup... constraintsOrGroups) {
        mandatory(property, NO_PERMISSIONS, constraintsOrGroups);
    }

    /**
     * Defines the property as mandatory if permissions match and all of the {@code ConditionsOrGroup}s are true.<p/>
     * I.e. the ConditionsOrGroup are ANDed where the PropConstraints within each ConditionsOrGroup are ORed.<p/>
     * E.g. [ConditionsGroup.OR(e, f), ConditionsGroup.OR(g, h)] is evaluated as: (e || f) && (g || h)
     *
     * @param property    the property name
     * @param permissions the permissions
     * @param constraintsOrGroups
     */
    public void mandatory(final String property, final Permissions permissions,
                          final ConditionsOrGroup... constraintsOrGroups) {
        mandatory(property, permissions, ConditionsTopGroup.AND(constraintsOrGroups));
    }

    /**
     * If the logical relation between the conditions are really complicated, this method may be your last resort.<p/>
     * This version defines the property as mandatory if the {@code ConditionsTopGroup} object evaluates to true.<p/>
     * According to the logical operation the ConditionsAndGroups resp. ConditionsOrGroups are either ANDed or ORed. <p/>
     * E.g. ConditionsTopGroup.AND(ConditionsGroup.OR(a, b), ConditionsGroup.OR(c, d), ConditionsGroup.OR(e, f)] is evaluated as: ...
     *
     * @param property
     * @param topGroup
     */
    public void mandatory(final String property, final ConditionsTopGroup topGroup) {
        mandatory(property, NO_PERMISSIONS, topGroup);
    }

    /**
     * If the logical relation between the conditions are really complicated, this method may be your last resort.<p/>
     * This version defines the property as mandatory if permissions match and the {@code ConditionsTopGroup} object
     * evaluates to true.<p/>
     * According to the logical operation the ConditionsAndGroups resp. ConditionsOrGroups are either ANDed or ORed.
     * <p/>
     * E.g. ConditionsTopGroup.AND(ConditionsGroup.OR(a, b), ConditionsGroup.OR(c, d), ConditionsGroup.OR(e, f)] is
     * evaluated as: ...
     * If the logical relation between the constraints are really complicated, this method may be your last resort.
     *
     * @param property
     * @param permissions
     * @param topGroup
     */
    public void mandatory(final String property, final Permissions permissions, final ConditionsTopGroup topGroup) {
        addPropertyConditions(property, NO_CONSTRAINT, permissions, topGroup, mandatoryConditionsMap.getOrInit(property));
    }


    public void immutable(final String property) {
        immutable(property, NO_PERMISSIONS);
    }

    public void immutable(final String property, final Permissions permissions) {
        immutable(property, permissions, NO_CONDITIONS_TOP_GROUP);
    }

    public void immutable(final String property, final PropConstraint... propConstraints) {
        immutable(property, NO_PERMISSIONS, propConstraints);
    }

    public void immutable(final String property, final Permissions permissions,
                          final PropConstraint... propConstraints) {
        immutable(property, permissions, ConditionsTopGroup.AND(ConditionsGroup.AND(propConstraints)));
    }

    public void immutable(final String property, final ConditionsAndGroup... conditionsAndGroups) {
        immutable(property, NO_PERMISSIONS, conditionsAndGroups);
    }

    public void immutable(final String property, final Permissions permissions,
                          final ConditionsAndGroup... conditionsAndGroups) {
        immutable(property, permissions, ConditionsTopGroup.OR(conditionsAndGroups));
    }

    public void immutable(final String property, final ConditionsOrGroup... constraintsOrGroups) {
        immutable(property, NO_PERMISSIONS, constraintsOrGroups);
    }

    public void immutable(final String property, final Permissions permissions,
                          final ConditionsOrGroup... constraintsOrGroups) {
        immutable(property, permissions, ConditionsTopGroup.AND(constraintsOrGroups));
    }

    public void immutable(final String property, final ConditionsTopGroup topGroup) {
        immutable(property, NO_PERMISSIONS, topGroup);
    }

    public void immutable(final String property, final Permissions permissions, final ConditionsTopGroup topGroup) {
        addPropertyConditions(property, NO_CONSTRAINT, permissions, topGroup, immutableConditionsMap.getOrInit(property));
    }


    /**
     * Defines the content constraint for this property.
     *
     * @param property
     * @param constraint
     */
    public void content(final String property, final ConstraintRoot constraint) {
        content(property, constraint, NO_PERMISSIONS);
    }

    /**
     * Defines the content constraint for this property if permissions match.
     * @param property
     * @param constraint
     * @param permissions
     */
    public void content(final String property, final ConstraintRoot constraint, final Permissions permissions) {
        content(property, constraint, permissions, ConditionsTopGroup.AND());
    }

    /**
     * Defines the content constraint for this property if all {@code PropConstraint}s are true.
     * I.e. the PropConstraints are ANDed. Convenience method for content(String, Constraint, ConditionsAndGroup).
     *
     * @param property
     * @param constraint
     * @param propConstraints
     */
    public void content(final String property, final ConstraintRoot constraint,
                        final PropConstraint... propConstraints) {
        content(property, constraint, NO_PERMISSIONS, propConstraints);
    }

    /**
     * Defines the content constraint for this property if permissions match and all {@code PropConstraint}s are
     * {@code true}. Convenience method for content(String, Constraint, Permissions, ConditionsAndGroup).
     *
     * @param property name the property this rule is defined for. According to the schema specification it might be a
     *                 simple, nested or indexed name.
     * @param constraint the constraint that applies to the value of this property.
     * @param permissions permissions that restrict the validity of the rule.
     * @param propConstraints one or more property related conditions that restrict the validity of the rule.
     */
    public void content(final String property, final ConstraintRoot constraint, final Permissions permissions,
                        final PropConstraint... propConstraints) {
        content(property, constraint, permissions, ConditionsTopGroup.AND(ConditionsGroup.AND(propConstraints)));
    }

    public void content(final String property, final ConstraintRoot constraint,
                        final ConditionsAndGroup... conditionsAndGroups) {
        content(property, constraint, NO_PERMISSIONS, conditionsAndGroups);
    }

    public void content(final String property, final ConstraintRoot constraint, final Permissions permissions,
                        final ConditionsAndGroup... conditionsAndGroups) {
        content(property, constraint, permissions, ConditionsTopGroup.OR(conditionsAndGroups));
    }

    public void content(final String property, final ConstraintRoot constraint,
                        final ConditionsOrGroup... constraintsOrGroups) {
        content(property, constraint, NO_PERMISSIONS, constraintsOrGroups);
    }

    public void content(final String property, final ConstraintRoot constraint, final Permissions permissions,
                        final ConditionsOrGroup... constraintsOrGroups) {
        content(property, constraint, permissions, ConditionsTopGroup.AND(constraintsOrGroups));
    }

    public void content(final String property, final ConstraintRoot constraint, final ConditionsTopGroup groups) {
        content(property, constraint, NO_PERMISSIONS, groups);
    }

    public void content(final String property, final ConstraintRoot constraint, final Permissions permissions,
                        final ConditionsTopGroup refTopGroup) {
        addPropertyConditions(property, constraint, permissions, refTopGroup, contentConditionsMap.getOrInit(property));
    }


    public void update(final String property, final ConstraintRoot constraint,
                       final PropConstraint... propConstraints) {
        update(property, constraint, NO_PERMISSIONS, propConstraints);
    }

    public void update(final String property, final ConstraintRoot constraint, final Permissions permissions,
                       final PropConstraint... propConstraints) {
        update(property, constraint, permissions, ConditionsTopGroup.AND(ConditionsGroup.AND(propConstraints)));
    }

    public void update(final String property, final ConstraintRoot constraint,
                       final ConditionsAndGroup... conditionsAndGroups) {
        update(property, constraint, NO_PERMISSIONS, conditionsAndGroups);
    }

    public void update(final String property, final ConstraintRoot constraint, final Permissions permissions,
                       final ConditionsAndGroup... conditionsAndGroups) {
        update(property, constraint, permissions, ConditionsTopGroup.OR(conditionsAndGroups));
    }

    public void update(final String property, final ConstraintRoot constraint,
                       final ConditionsOrGroup... constraintsOrGroups) {
        update(property, constraint, NO_PERMISSIONS, constraintsOrGroups);
    }

    public void update(final String property, final ConstraintRoot constraint, final Permissions permissions,
                       final ConditionsOrGroup... constraintsOrGroups) {
        update(property, constraint, permissions, ConditionsTopGroup.AND(constraintsOrGroups));
    }

    public void update(final String property, final ConstraintRoot constraint, final ConditionsTopGroup groups) {
        update(property, constraint, NO_PERMISSIONS, groups);
    }

    public void update(final String property, final ConstraintRoot constraint, final Permissions permissions,
                       final ConditionsTopGroup refTopGroup) {
        addPropertyConditions(property, constraint, permissions, refTopGroup, updateConditionsMap.getOrInit(property));
    }


    public void addPropertyConditions(final String property, final ConstraintRoot constraint, final Permissions permissions,
                                      final ConditionsTopGroup topGroup, List<Conditions> conditions) {
        Objects.requireNonNull(property, "property must not be null");
        Objects.requireNonNull(constraint, "constraint must not be null");
        Objects.requireNonNull(permissions, "permissions must not be null");
        Objects.requireNonNull(topGroup, "topGroup must not be null");
        if (property.isEmpty()) {
            throw new IllegalArgumentException("property must not be empty");
        }
        if (constraint != NO_CONSTRAINT) {
            validateConstraint(property, constraint);
        }
        validatePropertyAndConditions(property, topGroup);
        conditions.add(new Conditions(constraint, permissions, topGroup));
    }


    //TODO remove if permission validation makes no sense
    private void validatePermissions(Set<Permissions> permissionsMap, Permissions permissions, String propertyToLog) {
        permissions.validateValuesOrFail(null);
        // Check if any permissions are 'not unique'; e.g. Perm.any(A,B), followed by Perm.any(C,B) -> constraints for B
        // is not unique
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
                .map(ConstraintRoot::getValues)
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
            for (final PropConstraint propContraint : group.getPropConstraints()) {
                validatePropConstraint(propContraint);
            }
        }
    }

    private void validatePropConstraint(final PropConstraint propConstraint) {
        if (propConstraint == null) {
            throw new IllegalArgumentException("PropConstraint must not be null");
        }
        validateConstraint(propConstraint.getProperty(), propConstraint.getConstraint());
    }

    private void validateConstraint(final String property, final ConstraintRoot constraint) {
        if (property == null || constraint == null) {
            throw new IllegalArgumentException("Arguments must not be null");
        }
        final Class<?> propertyType = Validator.instance().validateProperty(property, typeClass);
        // Check that constraint supports propertyType
        if (!constraint.isSupportedType(propertyType)) {
            throw new IllegalArgumentException(
                    "Constraint " + constraint.getClass().getSimpleName() + " does not support type of property "
                            + property + " (" + propertyType + ")");
        }
        // Do further constraint specific validations
        //TODO have property and refProperty same type?
        constraint.validateValuesOrFail(typeClass);
    }


    /**
     * Overwrites the default key that is used as an identifier for the generic type {@code T} when the validation
     * rules are serialized to JSON. The default key is the lowercase simple name of the class {@code T}.
     *
     * @param typeJsonKey
     */
    public void setTypeJsonKey(final String typeJsonKey) {
        this.typeJsonKey = typeJsonKey;
    }

    public String getTypeJsonKey() {
        return typeJsonKey;
    }

    public static String serializeToJson(final ValidationRules<?>... rules) {
        final List<ValidationRules<?>> validationRulesList = Arrays.asList(rules);
        String json = asKey("schema-version") + quoted("0.3") + ",";
        json += asKey("mandatoryRules") + asObject(validationRulesList.stream()
                .map(ValidationRules::serializeMandatoryRules)
                .filter(j -> !j.isEmpty())
                .collect(Collectors.joining(","))) + ",";
        json += asKey("immutableRules") + asObject(validationRulesList.stream()
                .map(ValidationRules::serializeImmutableRules)
                .filter(j -> !j.isEmpty())
                .collect(Collectors.joining(","))) + ",";
        json += asKey("contentRules") + asObject(validationRulesList.stream()
                .map(ValidationRules::serializeContentRules)
                .filter(j -> !j.isEmpty())
                .collect(Collectors.joining(","))) + ",";
        json += asKey("updateRules") + asObject(validationRulesList.stream()
                .map(ValidationRules::serializeUpdateRules)
                .filter(j -> !j.isEmpty())
                .collect(Collectors.joining(",")));
        return asObject(json);
    }

    private String serializeMandatoryRules() {
        final String mapJson = mandatoryConditionsMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson) : "";
    }

    private String serializeImmutableRules() {
        final String mapJson = immutableConditionsMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson): "";
    }

    private String serializeContentRules() {
        final String mapJson = contentConditionsMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson): "";
    }

    private String serializeUpdateRules() {
        final String mapJson = updateConditionsMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson): "";
    }

}


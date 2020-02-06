package de.swa.clv;

import de.swa.clv.constraints.ConstraintRoot;
import de.swa.clv.constraints.PropConstraint;
import de.swa.clv.constraints.Permissions;
import de.swa.clv.groups.*;
import de.swa.clv.groups.RelationsSubGroup;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.asObject;
import static de.swa.clv.json.JsonUtil.quoted;

/**
 * A class to combine property validation rules for (possibly nested) properties of type {@code T},
 * providing some comfortable put methods, e.g. when more than one rule is needed. For each property a no-arg
 * getter method must exist.
 *
 * @param <T> the type for which the rules are defined
 */
public class ValidationRules<T> {

    public static final Permissions NO_PERMISSIONS_KEY = Permissions.any(new String[]{});
    public static final RelationsTopGroup NO_CONSTRAINT_REF_TOP_GROUP_VALUE = RelationsTopGroup.AND();

    private static final Logger log = LoggerFactory.getLogger(ValidationRules.class);

    private final Class<T> typeClass;

    private final PropertyMap mandatoryPropertyMap = new PropertyMap();
    private final PropertyMap immutablePropertyMap = new PropertyMap();
    private final ContentPropertyMap contentPropertyMap = new ContentPropertyMap();

    private String typeJsonKey;

    public ValidationRules(final Class<T> typeClass) {
        super();
        this.typeClass = typeClass;
        typeJsonKey = typeClass.getSimpleName().toLowerCase();
    }

    public Set<String> getMandatoryPropertyKeys() {
        return mandatoryPropertyMap.keySet();
    }

    public PermissionsMap getMandatoryPermissionsMap(final String property) {
        return mandatoryPropertyMap.get(property);
    }

    public Set<String> getImmutablePropertyKeys() {
        return immutablePropertyMap.keySet();
    }

    public PermissionsMap getImmutablePermissionsMap(final String property) {
        return immutablePropertyMap.get(property);
    }

    public Set<String> getContentPropertyKeys() {
        return contentPropertyMap.keySet();
    }

    public ContentPermissionsMap getContentPermissionsMap(final String property) {
        return contentPropertyMap.get(property);
    }

//    public Collection<ContentPermissionsMap> getContentPermissionMaps() {
//        return contentPropertyMap.getValues();
//    }

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
        mandatory(property, NO_PERMISSIONS_KEY);
    }

    /**
     * Defines the property as mandatory if permissions are met.
     *
     * @param property    the property name
     * @param permissions the permissions
     */
    public void mandatory(final String property, final Permissions permissions) {
        mandatory(property, permissions, NO_CONSTRAINT_REF_TOP_GROUP_VALUE);
    }

    /**
     * Defines the property as mandatory if all {@code constraintRefs} are true.
     * I.e. the ConstraintRefs are ANDed. Convenience method for mandatory(String, AndGroup).
     *
     * @param property       the property name
     * @param propConstraints
     */
    public void mandatory(final String property, final PropConstraint... propConstraints) {
        mandatory(property, NO_PERMISSIONS_KEY, propConstraints);
    }

    /**
     * Defines the property as mandatory if permissions are met and all {@code constraintRefs} are true.
     * I.e. the ConstraintRefs are ANDed. Convenience method for mandatory(String, Permissions, AndGroup).
     *
     * @param property       the property name
     * @param permissions    the permissions
     * @param propConstraints
     */
    public void mandatory(final String property, final Permissions permissions, final PropConstraint... propConstraints) {
        mandatory(property, permissions, RelationsTopGroup.AND(RelationsSubGroup.AND(propConstraints)));
    }

    /**
     * Defines the property as mandatory if at least one of the {@code andGroups} is true.<p/>
     * I.e. the AndGroups are ORed where the ConstrainRefs within each AndGroup are ANDed.<p/>
     * E.g. [Group.and(a, b), Group.and(c, d)] is evaluated as: a && b || c && d
     *
     * @param property
     * @param andGroups
     */
    public void mandatory(final String property, final AndGroup... andGroups) {
        mandatory(property, NO_PERMISSIONS_KEY, andGroups);
    }

    /**
     * Defines the property as mandatory if permissions are met and at least one of the {@code AndGroup}s is true.<p/>
     * I.e. the AndGroups are ORed where the ConstrainRefs within each AndGroup are ANDed.<p/>
     * E.g. [Group.and(a, b), Group.and(c, d)] is evaluated as: a && b || c && d
     *
     * @param property    the property name
     * @param permissions the permissions
     * @param andGroups
     */
    public void mandatory(final String property, final Permissions permissions, final AndGroup... andGroups) {
        mandatory(property, permissions, RelationsTopGroup.OR(andGroups));
    }

    /**
     * Defines the property as mandatory if all of the {@code OrGroup}s are true.<p/>
     * I.e. the OrGroups are ANDed where the ConstrainRefs within each OrGroup are ORed.<p/>
     * E.g. [Group.or(e, f), Group.or(g, h)] is evaluated as: (e || f) && (g || h)
     *
     * @param property    the property name
     * @param orGroups
     */
    public void mandatory(final String property, final OrGroup... orGroups) {
        mandatory(property, NO_PERMISSIONS_KEY, orGroups);
    }

    /**
     * Defines the property as mandatory if permissions are met and all of the {@code OrGroup}s are true.<p/>
     * I.e. the OrGroups are ANDed where the ConstrainRefs within each OrGroup are ORed.<p/>
     * E.g. [Group.or(e, f), Group.or(g, h)] is evaluated as: (e || f) && (g || h)
     *
     * @param property    the property name
     * @param permissions the permissions
     * @param orGroups
     */
    public void mandatory(final String property, final Permissions permissions, final OrGroup... orGroups) {
        mandatory(property, permissions, RelationsTopGroup.AND(orGroups));
    }

    /**
     * If the logical relation between the constraints are really complicated, this method may be your last resort.
     * <p/>
     * This version defines the property as mandatory if the {@code RelationsTopGroup} object evaluates to true.
     * <p/>
     * According to the logical operation the AndGroups and OrGroups are either ANDed or ORed.
     * <p/>
     * E.g. Groups.anded(Group.or(a, b), Group.or(c, d), Group.or(e, f)] is evaluated as: ...
     *
     * @param property
     * @param topGroup
     */
    public void mandatory(final String property, final RelationsTopGroup topGroup) {
        mandatory(property, NO_PERMISSIONS_KEY, topGroup);
    }

    /**
     * If the logical relation between the constraints are really complicated, this method may be your last resort.
     * <p/>
     * This version defines the property as mandatory if permissions are met and the {@code RelationsTopGroup} object evaluates to true.
     * <p/>
     * According to the logical operation the AndGroups and OrGroups are either ANDed or ORed.
     * <p/>
     * E.g. Groups.anded(Group.or(a, b), Group.or(c, d), Group.or(e, f)] is evaluated as: ...
     *
     * @param property
     * @param permissions
     * @param topGroup
     */
    public void mandatory(final String property, final Permissions permissions, final RelationsTopGroup topGroup) {
        putRulesToPropertyMap(mandatoryPropertyMap, property, permissions, topGroup);
    }


    public void immutable(final String property) {
        immutable(property, NO_PERMISSIONS_KEY);
    }

    public void immutable(final String property, final Permissions permissions) {
        immutable(property, permissions, NO_CONSTRAINT_REF_TOP_GROUP_VALUE);
    }

    public void immutable(final String property, final PropConstraint... propConstraints) {
        immutable(property, NO_PERMISSIONS_KEY, propConstraints);
    }

    public void immutable(final String property, final Permissions permissions, final PropConstraint... propConstraints) {
        immutable(property, permissions, RelationsTopGroup.AND(RelationsSubGroup.AND(propConstraints)));
    }

    public void immutable(final String property, final AndGroup... andGroups) {
        immutable(property, NO_PERMISSIONS_KEY, andGroups);
    }

    public void immutable(final String property, final Permissions permissions, final AndGroup... andGroups) {
        immutable(property, permissions, RelationsTopGroup.OR(andGroups));
    }

    public void immutable(final String property, final OrGroup... orGroups) {
        immutable(property, NO_PERMISSIONS_KEY, orGroups);
    }

    public void immutable(final String property, final Permissions permissions, final OrGroup... orGroups) {
        immutable(property, permissions, RelationsTopGroup.AND(orGroups));
    }

    public void immutable(final String property, final RelationsTopGroup topGroup) {
        immutable(property, NO_PERMISSIONS_KEY, topGroup);
    }

    public void immutable(final String property, final Permissions permissions, final RelationsTopGroup topGroup) {
        putRulesToPropertyMap(immutablePropertyMap, property, permissions, topGroup);
    }

    private void putRulesToPropertyMap(final PropertyMap propertyMap, final String property, final Permissions permissions,
                                            final RelationsTopGroup topGroup) {
        Objects.requireNonNull(propertyMap, "propertyMap must not be null");
        Objects.requireNonNull(property, "property must not be null");
        Objects.requireNonNull(permissions, "permissions must not be null");
        Objects.requireNonNull(topGroup, "topGroup must not be null");

        final PermissionsMap permissionsMap = propertyMap.getOrInit(property);

        validatePermissions(permissionsMap.keySet(), permissions, property);
        validatePropertyAndConstraintRefs(property, topGroup);

        permissionsMap.put(permissions, topGroup);
    }


    /**
     * Defines the constant contentConstraint for this property.
     *
     * @param property
     * @param contentConstraint
     */
    public void content(final String property, final ConstraintRoot contentConstraint) {
        content(property, contentConstraint, NO_PERMISSIONS_KEY);
    }

    /**
     * Defines the constant contentConstraint for this property and permissions.
     *  @param property
     * @param contentConstraint
     * @param permissions
     */
    public void content(final String property, final ConstraintRoot contentConstraint, final Permissions permissions) {
        content(property, contentConstraint, permissions, RelationsTopGroup.AND());
    }

    /**
     * Defines the content constraint for this property if all {@code constraintRefs} are true.
     * I.e. the ConstraintRefs are ANDed. Convenience method for content(String, Constraint, AndGroup).
     *
     * @param property
     * @param constraint
     * @param propConstraints
     */
    public void content(final String property, final ConstraintRoot constraint, final PropConstraint... propConstraints) {
        content(property, constraint, NO_PERMISSIONS_KEY, propConstraints);
    }

    /**
     * Defines the content constraint for this property and permissions if all {@code constraintRefs} are true.
     * I.e. the ConstraintRefs are ANDed. Convenience method for content(String, Permissions, Constraint, AndGroup).
     *  @param property
     * @param constraint
     * @param permissions
     * @param propConstraints
     */
    public void content(final String property, final ConstraintRoot constraint, final Permissions permissions, final PropConstraint... propConstraints) {
        content(property, constraint, permissions, RelationsTopGroup.AND(RelationsSubGroup.AND(propConstraints)));
    }

    /**
     * Defines the content constraint for this property if at least one of the {@code constraintRefGroups} is true.<p/>
     * I.e. the AndGroups are ORed where the ConstrainRefs within each AndGroup are ANDed.<p/>
     * E.g. [Group.and(a, b), Group.and(c, d)] is evaluated as: a && b || c && d
     *
     * @param property
     * @param constraint
     * @param andGroups
     */
    public void content(final String property, final ConstraintRoot constraint, final AndGroup... andGroups) {
        content(property, constraint, NO_PERMISSIONS_KEY, andGroups);
    }

    /**
     * Defines the content constraint for this property and permissions if at least one of the {@code constraintRefGroups} is true.<p/>
     * I.e. the AndGroups are ORed where the ConstrainRefs within each AndGroup are ANDed.<p/>
     * E.g. [Group.and(a, b), Group.and(c, d)] is evaluated as: a && b || c && d
     *
     * @param property
     * @param constraint
     * @param permissions
     * @param andGroups
     */
    public void content(final String property, final ConstraintRoot constraint, final Permissions permissions, final AndGroup... andGroups) {
        content(property, constraint, permissions, RelationsTopGroup.OR(andGroups));
    }

    /**
     * Defines the content constraint for this property if all of the {@code constraintRefGroups} is true.<p/>
     * I.e. the OrGroups are ANDed where the ConstrainRefs within each OrGroup are ORed.<p/>
     * E.g. [Group.or(e, f), Group.or(g, h)] is evaluated as: (e || f) && (g || h)
     *
     * @param property
     * @param constraint
     * @param orGroups
     */
    public void content(final String property, final ConstraintRoot constraint, final OrGroup... orGroups) {
        content(property, constraint, NO_PERMISSIONS_KEY, orGroups);
    }

    /**
     * Defines the content constraint for this property and permissions if all of the {@code constraintRefGroups} is true.<p/>
     * I.e. the OrGroups are ANDed where the ConstrainRefs within each OrGroup are ORed.<p/>
     * E.g. [Group.or(e, f), Group.or(g, h)] is evaluated as: (e || f) && (g || h)
     *
     * @param property
     * @param constraint
     * @param permissions
     * @param orGroups
     */
    public void content(final String property, final ConstraintRoot constraint, final Permissions permissions, final OrGroup... orGroups) {
        content(property, constraint, permissions, RelationsTopGroup.AND(orGroups));
    }

    /**
     * If the logical relation between the constraints are really complicated, this method may be your last resort.
     * <p/>
     * This version defines the content constraint for this property if the {@code groups} object evaluates to true.
     * <p/>
     * According to the logical operation the AndGroups and OrGroups are either ANDed or ORed.
     * <p/>
     * E.g. Groups.anded(Group.or(a, b), Group.or(c, d), Group.or(e, f)] is evaluated as: ...
     *
     * @param property
     * @param constraint
     * @param groups
     */
    public void content(final String property, final ConstraintRoot constraint, final RelationsTopGroup groups) {
        content(property, constraint, NO_PERMISSIONS_KEY, groups);
    }

    /**
     * If the logical relation between the constraints are really complicated, this method may be your last resort.
     * <p/>
     * This version defines the content contentConstraint for this property if the permissions are met and the {@code topGroup} object evaluates to true.
     * <p/>
     * According to the logical operation the AndGroups and OrGroups are either ANDed or ORed.
     * <p/>
     * E.g. Groups.anded(Group.or(a, b), Group.or(c, d), Group.or(e, f)] is evaluated as: ...
     *
     * @param property
     * @param contentConstraint
     * @param permissions
     * @param topGroup
     */
    public void content(final String property, final ConstraintRoot contentConstraint, final Permissions permissions,
                        final RelationsTopGroup topGroup) {
        Objects.requireNonNull(property, "property must not be null");
        Objects.requireNonNull(contentConstraint, "contentConstraint must not be null");
        Objects.requireNonNull(permissions, "permissions must not be null");
        Objects.requireNonNull(topGroup, "topGroup must not be null");
        if (property.isEmpty()) {
            throw new IllegalArgumentException("property must not be empty");
        }

        final ContentPermissionsMap permissionsMap = contentPropertyMap.getOrInit(property);

        validatePermissions(permissionsMap.keySet(), permissions, property);
        validateConstraint(property, contentConstraint);
        validatePropertyAndConstraintRefs(property, topGroup);

        permissionsMap.put(permissions, new ContentConstraints(contentConstraint, topGroup));
    }


    private void validatePermissions(Set<Permissions> permissionsMap, Permissions permissions, String property) {
        permissions.validateValuesOrFail(null);
        // Check if any permissions are 'not unique'; e.g. Perm.any(A,B), followed by Perm.any(C,B) -> constraints for B is not unique
        if (permissions == NO_PERMISSIONS_KEY) {
            if (permissionsMap.contains(permissions)) {
                throw new IllegalArgumentException(String.format("Validation rules for property '%s' (w/o permissions) are already defined.", property));
            }
        } else {
            if(permissions.getValues().isEmpty()) {
                throw new IllegalArgumentException(String.format("Permissions for property '%s' must not be empty.", property));
            }
            Optional<String> nonUniquePermission = checkPermissionUniqueness(permissionsMap, permissions);
            if (nonUniquePermission.isPresent()) {
                throw new IllegalArgumentException(String.format("Validation rules for property '%s' and permission '%s' are already defined.",
                        property, nonUniquePermission.get().toString()));
            }
        }
    }

    private Optional<String> checkPermissionUniqueness(Set<Permissions> existingPermissions, Permissions newPermissions) {
        // Flatten all permission values
        final Set<String> existingPerms = existingPermissions.stream()
                .map(p -> p.getValues())
                .flatMap(Collection::stream)
                .map(p -> p.toString())
                .collect(Collectors.toSet());
        // Search for permission that is already defined
        final Set<String> newPermissionsAsSet = newPermissions.getValues().stream()
                .map(p -> p.toString())
                .collect(Collectors.toSet());
        existingPerms.retainAll(newPermissionsAsSet);
        return existingPerms.stream().findFirst();
    }

    private void validatePropertyAndConstraintRefs(String property, RelationsTopGroup topGroup) {
        Validator.instance().validateProperty(property, typeClass);
        for (final RelationsSubGroup group : topGroup.getRelationsSubGroups()) {
            for (final PropConstraint ref : group.getPropConstraints()) {
                validateConstraintRef(ref);
            }
        }
    }

    private void validateConstraintRef(final PropConstraint propConstraint) {
        if (propConstraint == null) {
            throw new IllegalArgumentException("ConstraintRef must not be null");
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
        //TODO haben property und refProperty den selben Typ ...
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
        String json = asKey("schema-version") + quoted("0.1") + ",";
        json += asKey("mandatoryRules") + asObject(validationRulesList.stream()
                .map(c -> c.serializeMandatoryRules())
                .filter(j -> !j.isEmpty())
                .collect(Collectors.joining(","))) + ",";
        json += asKey("immutableRules") + asObject(validationRulesList.stream()
                .map(c -> c.serializeImmutableRules())
                .filter(j -> !j.isEmpty())
                .collect(Collectors.joining(","))) + ",";
        json += asKey("contentRules") + asObject(validationRulesList.stream()
                .map(c -> c.serializeContentRules())
                .filter(j -> !j.isEmpty())
                .collect(Collectors.joining(",")));
        return asObject(json);
    }

    private String serializeMandatoryRules() {
        final String mapJson = mandatoryPropertyMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson) : "";
    }

    private String serializeImmutableRules() {
        final String mapJson = immutablePropertyMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson): "";
    }

    private String serializeContentRules() {
        final String mapJson = contentPropertyMap.serializeToJson();
        return !mapJson.isEmpty() ? asKey(typeJsonKey) + asObject(mapJson): "";
    }

}


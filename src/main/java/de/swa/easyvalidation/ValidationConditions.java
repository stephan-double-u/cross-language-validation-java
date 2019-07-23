package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.constraints.ConstraintRef;
import de.swa.easyvalidation.constraints.Dates;
import de.swa.easyvalidation.constraints.Permissions;
import de.swa.easyvalidation.groups.AndGroup;
import de.swa.easyvalidation.groups.ConstraintRefGroup;
import de.swa.easyvalidation.groups.ConstraintRefTopGroup;
import de.swa.easyvalidation.groups.ContentContraintGroup;
import de.swa.easyvalidation.groups.OrGroup;
import de.swa.easyvalidation.json.JsonSerializable;
import de.swa.easyvalidation.json.JsonUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static javax.swing.plaf.basic.BasicHTML.propertyKey;

/**
 * A class to combine property validation conditions for (possibly nested) properties of type {@code T},
 * providing some comfortable put methods, e.g. when more than one condition is needed. For each property a no-arg
 * getter method must exist.
 *
 * @param <T> the type for which the conditions are defined
 */
public class ValidationConditions<T> implements JsonSerializable {

    public static final Permissions NO_PERMISSIONS_NULL_KEY = Permissions.any(new String[]{});

    private static final Logger log = LoggerFactory.getLogger(ValidationConditions.class);
    private final Class<T> typeClass;

    private final PropertyMap mandatoryPropertyMap = new PropertyMap();
    private final PropertyMap immutablePropertyMap = new PropertyMap();
    private final ContentPropertyMap contentPropertyMap = new ContentPropertyMap();

    private String typeJsonKey;

    public ValidationConditions(final Class<T> typeClass) {
        super();
        this.typeClass = typeClass;
        typeJsonKey = typeClass.getSimpleName().toLowerCase();
    }

    public Set<String> getMandatoryPropertyKeys() {
        return mandatoryPropertyMap.getKeys();
    }

    public Map<Permissions, ConstraintRefTopGroup> getMandatoryPermissionsMap(final String property) {
        return mandatoryPropertyMap.getOrInitPermissionsMap(property);
    }

    public Set<String> getImmutablePropertyKeys() {
        return immutablePropertyMap.getKeys();
    }

    public Map<Permissions, ConstraintRefTopGroup> getImmutablePermissionsMap(final String property) {
        return immutablePropertyMap.getOrInitPermissionsMap(property);
    }

    public Set<String> getContentPropertyKeys() {
        return contentPropertyMap.getKeys();
    }

    public Map<Permissions, ContentContraintGroup> getContentPermissionsMap(final Object property) {
        return contentPropertyMap.getPermissionsMap(property);
    }

    public Collection<Map<Permissions, ContentContraintGroup>> getContentPermissionMaps() {
        return contentPropertyMap.getValues();
    }


    /**
     * Defines the property as mandatory.
     *
     * @param property the property name
     */
    public void mandatory(final String property) {
        mandatory(property, NO_PERMISSIONS_NULL_KEY);
    }

    /**
     * Defines the property as mandatory if permissions are met.
     *
     * @param property    the property name
     * @param permissions the permissions
     */
    public void mandatory(final String property, final Permissions permissions) {
        mandatory(property, permissions, ConstraintRefTopGroup.anded());
    }

    /**
     * Defines the property as mandatory if all {@code constraintRefs} are true.
     * I.e. the ConstraintRefs are ANDed. Convenience method for mandatory(String, AndGroup).
     *
     * @param property       the property name
     * @param constraintRefs
     */
    public void mandatory(final String property, final ConstraintRef... constraintRefs) {
        mandatory(property, null, constraintRefs);
    }

    /**
     * Defines the property as mandatory if permissions are met and all {@code constraintRefs} are true.
     * I.e. the ConstraintRefs are ANDed. Convenience method for mandatory(String, Permissions, AndGroup).
     *
     * @param property       the property name
     * @param permissions    the permissions
     * @param constraintRefs
     */
    public void mandatory(final String property, final Permissions permissions, final ConstraintRef... constraintRefs) {
        mandatory(property, permissions, ConstraintRefTopGroup.anded(ConstraintRefGroup.and(constraintRefs)));
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
        mandatory(property, null, andGroups);
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
        mandatory(property, permissions, ConstraintRefTopGroup.ored(andGroups));
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
        mandatory(property, null, orGroups);
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
        mandatory(property, permissions, ConstraintRefTopGroup.anded(orGroups));
    }

    /**
     * If the logical relation between the constraints are really complicated, this method may be your last resort.
     * <p/>
     * This version defines the property as mandatory if the {@code ConstraintRefTopGroup} object evaluates to true.
     * <p/>
     * According to the logical operation the AndGroups and OrGroups are either ANDed or ORed.
     * <p/>
     * E.g. Groups.anded(Group.or(a, b), Group.or(c, d), Group.or(e, f)] is evaluated as: ...
     *
     * @param property
     * @param groups
     */
    public void mandatory(final String property, final ConstraintRefTopGroup groups) {
        mandatory(property, null, groups);
    }

    /**
     * If the logical relation between the constraints are really complicated, this method may be your last resort.
     * <p/>
     * This version defines the property as mandatory if permissions are met and the {@code ConstraintRefTopGroup} object evaluates to true.
     * <p/>
     * According to the logical operation the AndGroups and OrGroups are either ANDed or ORed.
     * <p/>
     * E.g. Groups.anded(Group.or(a, b), Group.or(c, d), Group.or(e, f)] is evaluated as: ...
     *
     * @param property
     * @param permissions
     * @param groups
     */
    public void mandatory(final String property, final Permissions permissions, final ConstraintRefTopGroup groups) {
        putConditions(mandatoryPropertyMap, property, permissions, groups);
    }


    public void immutable(final String property) {
        immutable(property, NO_PERMISSIONS_NULL_KEY);
    }

    public void immutable(final String property, final Permissions permissions) {
        immutable(property, permissions, ConstraintRefTopGroup.anded());
    }

    public void immutable(final String property, final ConstraintRef... constraintRefs) {
        immutable(property, null, constraintRefs);
    }

    public void immutable(final String property, final Permissions permissions, final ConstraintRef... constraintRefs) {
        immutable(property, permissions, ConstraintRefTopGroup.anded(ConstraintRefGroup.and(constraintRefs)));
    }

    public void immutable(final String property, final AndGroup... andGroups) {
        immutable(property, null, andGroups);
    }

    public void immutable(final String property, final Permissions permissions, final AndGroup... andGroups) {
        immutable(property, permissions, ConstraintRefTopGroup.ored(andGroups));
    }

    public void immutable(final String property, final OrGroup... orGroups) {
        immutable(property, null, orGroups);
    }

    public void immutable(final String property, final Permissions permissions, final OrGroup... orGroups) {
        immutable(property, permissions, ConstraintRefTopGroup.anded(orGroups));
    }

    public void immutable(final String property, final ConstraintRefTopGroup groups) {
        putConditions(immutablePropertyMap, property, null, groups);
    }

    public void immutable(final String property, final Permissions permissions, final ConstraintRefTopGroup groups) {
        putConditions(immutablePropertyMap, property, permissions, groups);
    }


    private void putConditions(final PropertyMap propertyMap, final String property, final Permissions permissions, final ConstraintRefTopGroup topGroup) {

        final Permissions permissionsNonNull = (permissions == null) ? NO_PERMISSIONS_NULL_KEY : permissions;
        final Map<Permissions, ConstraintRefTopGroup> permissionsMap = propertyMap.getOrInitPermissionsMap(property);

        validatePermissionsOrFail(permissionsMap.keySet(), permissionsNonNull, property);
        validatePropertyAndContraints(property, topGroup);

        permissionsMap.put(permissions, topGroup);
    }

    private void validatePermissionsOrFail(Set<Permissions> permissionsMap, Permissions permissions, String property) {
        // Check if any permissions are 'not unique'; e.g. Perm.any(A,B), followed by Perm.any(C,B) -> constraints for B is not unique
        if (permissions == NO_PERMISSIONS_NULL_KEY) {
            if (permissionsMap.contains(permissions)) {
                throw new IllegalArgumentException(String.format("Validation conditions for property '%s' (w/o permissions) are already defined.", property));
            }
        } else {
            Optional<Object> nonUniquePermission = checkPermissionUniqueness(permissionsMap, permissions);
            if (nonUniquePermission.isPresent()) {
                throw new IllegalArgumentException(String.format("Validation conditions for property '%s' and permission '%s' are already defined.",
                        property, nonUniquePermission.get().toString()));
            }
        }
    }

    private void validatePropertyAndContraints(String property, ConstraintRefTopGroup topGroup) {
        EasyValidator.validateProperty(property, typeClass);
        for (final ConstraintRefGroup group : topGroup.getConstraintRefGroups()) {
            for (final ConstraintRef ref : group.getConstraintRefs()) {
                validatePropertyAndValueTypes(ref);
            }
        }
    }

    private Optional<Object> checkPermissionUniqueness(Set<Permissions> permissionsSet, Permissions permissions) {
        // Flatten all permission values
        final Set<Object> existingPerms = permissionsSet.stream().map(p -> p.getValues()).flatMap(Collection::stream).collect(Collectors.toSet());

        final Set<Object> newPermissions = permissions.getValues().stream().collect(Collectors.toSet());
        existingPerms.retainAll(newPermissions);
        return existingPerms.stream().findFirst();
    }

    /*
     * Content related methods
     */

    /**
     * Defines the constant constraint for this property.
     *
     * @param property
     * @param constraint
     */
    public void content(final String property, final Constraint constraint) {
        content(property, null, constraint);
    }

    /**
     * Defines the constant constraint for this property and permissions.
     *
     * @param property
     * @param permissions
     * @param constraint
     */
    public void content(final String property, final Permissions permissions, final Constraint constraint) {
        content(property, permissions, constraint, ConstraintRefTopGroup.anded());
    }

    /**
     * Defines the content constraint for this property if all {@code constraintRefs} are true.
     * I.e. the ConstraintRefs are ANDed. Convenience method for content(String, Constraint, AndGroup).
     *
     * @param property
     * @param constraint
     * @param constraintRefs
     */
    public void content(final String property, final Constraint constraint, final ConstraintRef... constraintRefs) {
        content(property, null, constraint, constraintRefs);
    }

    /**
     * Defines the content constraint for this property and permissions if all {@code constraintRefs} are true.
     * I.e. the ConstraintRefs are ANDed. Convenience method for content(String, Permissions, Constraint, AndGroup).
     *
     * @param property
     * @param permissions
     * @param constraint
     * @param constraintRefs
     */
    public void content(final String property, final Permissions permissions, final Constraint constraint, final ConstraintRef... constraintRefs) {
        content(property, permissions, constraint, ConstraintRefTopGroup.anded(ConstraintRefGroup.and(constraintRefs)));
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
    public void content(final String property, final Constraint constraint, final AndGroup... andGroups) {
        content(property, null, constraint, andGroups);
    }

    /**
     * Defines the content constraint for this property and permissions if at least one of the {@code constraintRefGroups} is true.<p/>
     * I.e. the AndGroups are ORed where the ConstrainRefs within each AndGroup are ANDed.<p/>
     * E.g. [Group.and(a, b), Group.and(c, d)] is evaluated as: a && b || c && d
     *
     * @param property
     * @param permissions
     * @param constraint
     * @param andGroups
     */
    public void content(final String property, final Permissions permissions, final Constraint constraint, final AndGroup... andGroups) {
        content(property, permissions, constraint, ConstraintRefTopGroup.ored(andGroups));
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
    public void content(final String property, final Constraint constraint, final OrGroup... orGroups) {
        content(property, null, constraint, orGroups);
    }

    /**
     * Defines the content constraint for this property and permissions if all of the {@code constraintRefGroups} is true.<p/>
     * I.e. the OrGroups are ANDed where the ConstrainRefs within each OrGroup are ORed.<p/>
     * E.g. [Group.or(e, f), Group.or(g, h)] is evaluated as: (e || f) && (g || h)
     *
     * @param property
     * @param permissions
     * @param constraint
     * @param orGroups
     */
    public void content(final String property, final Permissions permissions, final Constraint constraint, final OrGroup... orGroups) {
        content(property, permissions, constraint, ConstraintRefTopGroup.anded(orGroups));
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
    public void content(final String property, final Constraint constraint, final ConstraintRefTopGroup groups) {
        content(property, null, constraint, groups);
    }

    /**
     * If the logical relation between the constraints are really complicated, this method may be your last resort.
     * <p/>
     * This version defines the content constraint for this property if the permissions are met and the {@code groups} object evaluates to true.
     * <p/>
     * According to the logical operation the AndGroups and OrGroups are either ANDed or ORed.
     * <p/>
     * E.g. Groups.anded(Group.or(a, b), Group.or(c, d), Group.or(e, f)] is evaluated as: ...
     *
     * @param property
     * @param permissions
     * @param constraint
     * @param groups
     */
    public void content(final String property, final Permissions permissions, final Constraint constraint,
                        final ConstraintRefTopGroup groups) {
        //TODO check params ...
        EasyValidator.validateProperty(property, typeClass);
        constraint.validateArgumentsOrFail(typeClass);
        for (final ConstraintRefGroup group : groups.getConstraintRefGroups()) {
            for (final ConstraintRef ref : group.getConstraintRefs()) {
                validatePropertyAndValueTypes(ref);
            }
        }
        putContentConditions(property, permissions, constraint, groups);
    }

    private void putContentConditions(final String property, final Permissions permissions, final Constraint constraint, final ConstraintRefTopGroup constraintRefTopGroup) {
        if (getContentPermissionsMap(property) == null) {
            contentPropertyMap.putPermissionsMap(property, new LinkedHashMap<>());
        }
        final Map<Permissions, ContentContraintGroup> permissionsMap = contentPropertyMap.getPermissionsMap(property);
        final Permissions permissionsParam = (permissions == null) ? NO_PERMISSIONS_NULL_KEY : permissions;

        putContentConditions(permissionsMap, permissionsParam, constraint, constraintRefTopGroup, property);
    }

    private void putContentConditions(final Map<Permissions, ContentContraintGroup> permissionsMap, final Permissions permissions, final Constraint constraint,
                                      final ConstraintRefTopGroup constraintRefTopGroup, final String property) {
        //TODO check if any permissions are 'not unique'; e.g. Perm.any(A,B), followed by Perm.any(C,B) -> constraints for B is not unique
        final ContentContraintGroup contentContraintGroup = permissionsMap.get(permissions);
        if (contentContraintGroup != null) {
            throw new IllegalArgumentException(
                    String.format("Validation conditions for property '{}' and permissions '{}' are already defined and will be overwritten", property, permissions));
        }
        permissionsMap.put(permissions, new ContentContraintGroup(constraint, constraintRefTopGroup));
    }

    private void validatePropertyAndValueTypes(final ConstraintRef constraintRef) {
        if (constraintRef == null) {
            throw new IllegalArgumentException("ConstraintRef null is not allowed");
        }
        final String propertyName = constraintRef.getProperty();
        final Class<?> propertyType = EasyValidator.validateProperty(propertyName, typeClass);
        final Constraint constraint = constraintRef.getConstraint();

        // Check that constraint supports propertyType
        if (!constraint.isSupportedType(propertyType)) {
            throw new IllegalArgumentException(
                    "Contraint " + constraint.getClass().getSimpleName() + " does not support type of property "
                            + propertyName + " (" + propertyType + ")");
        }
        // Check arguments
        constraint.validateArgumentsOrFail(propertyType);
    }

    /**
     * Overwrites the default key that is used as an identifier for the generic type {@code T} when this map is
     * serialized to JSON. The default key is the lowercase simple name of the class {@code T}.
     *
     * @param typeJsonKey
     */
    public void setTypeJsonKey(final String typeJsonKey) {
        this.typeJsonKey = typeJsonKey;
    }

    @Override
    public String serializeToJson() {
        String json = "{" + JsonUtil.asKey(typeJsonKey) + "{";
        json += JsonUtil.asKey("mandatory") + JsonUtil.asObject(serializeConditions(mandatoryPropertyMap)) + ",";
        json += JsonUtil.asKey("immutable") + JsonUtil.asObject(serializeConditions(immutablePropertyMap)) + ",";
        //json += JsonUtil.asKey("content") + JsonUtil.asObject(serializeCondition(content));
        json += "}}";
        return json;
    }

    private String serializeConditions(final PropertyMap propertyMap) {
        String json = "";
        boolean firstProp = true;
        for (final String propertyKey : propertyMap.getKeys()) {
            final Map<Permissions, ConstraintRefTopGroup> permissionsMap = propertyMap.getOrInitPermissionsMap(propertyKey);
            // TODO permissions ...
            for (final Permissions permissions : permissionsMap.keySet()) {
                final ConstraintRefTopGroup constraintRefTopGroup = permissionsMap.get(permissions);
                final String logicalOperator = constraintRefTopGroup.getLogicalOperator().name();
                json += (firstProp ? "" : ",") + JsonUtil.asKey(propertyKey);
                if (constraintRefTopGroup.getConstraintRefGroups().length == 0) {
                    json += "true";
                } else {
                    json += "{" + JsonUtil.asKey("groupsOperator") + JsonUtil.quoted(logicalOperator) + "," + JsonUtil.asKey("permissionsMap") + "[";
                    boolean firstGroup = true;
                    for (final ConstraintRefGroup group : constraintRefTopGroup.getConstraintRefGroups()) {
                        json += (firstGroup ? "" : ",") + group.serializeToJson();
                        firstGroup = false;
                        group.serializeToJson();
                    }
                    json += "]}";
                }
                firstProp = false;
            }
        }
        return json;
    }

    private String serializeConditions(final Map<String, ConstraintRefTopGroup> conditionGroupsMap) {
        String json = "";
        boolean firstProp = true;
        for (final String propertyKey : conditionGroupsMap.keySet()) {
            final ConstraintRefTopGroup topGroup = conditionGroupsMap.get(propertyKey);
            final String logicalOperator = topGroup.getLogicalOperator().name();
            json += (firstProp ? "" : ",") + JsonUtil.asKey(propertyKey);
            if (topGroup.getConstraintRefGroups().length == 0) {
                json += "true";
            } else {
                json += "{" + JsonUtil.asKey("groupsOperator") + JsonUtil.quoted(logicalOperator) + "," + JsonUtil.asKey("topGroup") + "[";
                boolean firstGroup = true;
                for (final ConstraintRefGroup group : topGroup.getConstraintRefGroups()) {
                    json += (firstGroup ? "" : ",") + group.serializeToJson();
                    firstGroup = false;
                    group.serializeToJson();
                }
                json += "]}";
            }
            firstProp = false;
        }
        return json;
    }

    private String serializeCondition(final Map<String, ContentContraintGroup> content) {
        final String json = "";
        final boolean firstProp = true;
        for (final String propertyKey : content.keySet()) {
            final ContentContraintGroup contentContraintGroup = content.get(propertyKey);
            final Constraint contentConstraint = contentContraintGroup.getContentConstraint();
            final ConstraintRefTopGroup groups = contentContraintGroup.getConstraintRefTopGroup();
            final String logicalOperator = groups.getLogicalOperator().name();
            // ...

        }
        return "TODO";
    }
}

class PropertyMap {
    // <property> -> {<permissions> -> ConstraintRefTopGroup}
    // Linked HashMap to preserve insertion order and thereby define validation order, e.g. to do cheap validations first!
    // -> no need for javax.validation.GroupSequence!
    private final Map<String, Map<Permissions, ConstraintRefTopGroup>> map = new LinkedHashMap<>();

    public Set<String> getKeys() {
        return map.keySet();
    }

    public Map<Permissions, ConstraintRefTopGroup> getOrInitPermissionsMap(final String property) {
        if (map.get(property) == null) {
            map.put(property, new LinkedHashMap<>());
        }
        return map.get(property);
    }
}

class ContentPropertyMap {
    // <property> -> {<permissions> -> ContentContraintGroup}
    private final Map<String, Map<Permissions, ContentContraintGroup>> map = new LinkedHashMap<>();

    public Set<String> getKeys() {
        return map.keySet();
    }

    public Collection<Map<Permissions, ContentContraintGroup>> getValues() {
        return map.values();
    }

    public Map<Permissions, ContentContraintGroup> getPermissionsMap(final Object property) {
        return map.get(property);
    }

    public Map<Permissions, ContentContraintGroup> putPermissionsMap(final String property, final Map<Permissions, ContentContraintGroup> permissionsMap) {
        return map.put(property, permissionsMap);
    }
}


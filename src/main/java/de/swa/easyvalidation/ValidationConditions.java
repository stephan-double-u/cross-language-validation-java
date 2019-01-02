package de.swa.easyvalidation;

import static de.swa.easyvalidation.json.JsonUtil.*;
import static de.swa.easyvalidation.json.JsonUtil.quoted;

import java.util.LinkedHashMap;
import java.util.Map;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.constraints.ConstraintRef;
import de.swa.easyvalidation.groups.AndGroup;
import de.swa.easyvalidation.groups.ConstraintRefGroup;
import de.swa.easyvalidation.groups.ConstraintRefGroups;
import de.swa.easyvalidation.groups.ContentGroup;
import de.swa.easyvalidation.groups.OrGroup;
import de.swa.easyvalidation.json.JsonSerializable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A collection class to combine property validation conditions for (possibly nested) properties of type {@code T},
 * providing some comfortable put methods, e.g. when more than one condition is needed. For each property a no-arg
 * getter method must exist.
 *
 * @author stwa1de
 *
 * @param <T>
 *            the type for which the conditions are defined
 */
public class ValidationConditions<T> implements JsonSerializable {

    static Logger log = LoggerFactory.getLogger(ValidationConditions.class);

    private Class<T> typeClass;
    private String typeJsonKey;

    // Linked HashMap to preserve insertion order and thereby define validation order, e.g. to do cheap validations
    // fist! -> no need for javax.validation.GroupSequence !
    private Map<String, ConstraintRefGroups> mandatory = new LinkedHashMap<>();
    private Map<String, ConstraintRefGroups> immutable = new LinkedHashMap<>();
    private Map<String, ContentGroup> content = new LinkedHashMap<>();

    public ValidationConditions(Class<T> typeClass) {
        super();
        this.typeClass = typeClass;
        this.typeJsonKey = typeClass.getSimpleName().toLowerCase();
    }

    /**
     * Defines the property as mandatory.
     * 
     * @param property
     *            the mandatory property
     */
    public void mandatory(String property) {
        putCondition(mandatory, property);
    }

    /**
     * Defines the property as immutable.
     * 
     * @param property
     *            the immutable property
     */
    public void immutable(String property) {
        putCondition(immutable, property);
    }
    
    private void putCondition(Map<String, ConstraintRefGroups> conditionMap, String property) {
        EasyValidator.validateProperty(property, typeClass);
        putAndWarn(conditionMap, property, ConstraintRefGroups.anded());
    }
    
    /**
     * Defines the property as mandatory if all {@code constraintRefs} are true.
     * I.e. the ConstraintRefs are ANDed. Convenience method for mandatory(String, AndGroup).
     * 
     * @param property
     * @param constraintRefs
     */
    public void mandatory(String property, ConstraintRef... constraintRefs) {
        putCondition(mandatory, property, constraintRefs);
    }

    /**
     * Defines the property as immutable if all {@code constraintRefs} are true.
     * I.e. the ConstraintRefs are ANDed. Convenience method for immutable(String, AndGroup).
     * 
     * @param property
     * @param constraintRefs
     */
    public void immutable(String property, ConstraintRef... constraintRefs) {
        putCondition(immutable, property, constraintRefs);
    }
    
    private void putCondition(Map<String, ConstraintRefGroups> conditionMap, String property, ConstraintRef... constraintRefs) {
        EasyValidator.validateProperty(property, typeClass);
        for (ConstraintRef ref : constraintRefs) {
            validatePropertyAndValueTypes(ref);
        }
        putAndWarn(conditionMap, property, ConstraintRefGroups.anded(ConstraintRefGroup.and(constraintRefs)));
    }
    
    /**
     * Defines the property as mandatory if at least one of the {@code constraintRefGroups} is true.<p/>
     * I.e. the AndGroups are ORed where the ConstrainRefs within each AndGroup are ANDed.<p/>
     * E.g. [Group.and(a, b), Group.and(c, d)] is evaluated as: a && b || c && d
     * 
     * @param property
     * @param constraintRefGroups
     */
    public void mandatory(String property, AndGroup... constraintRefGroups) {
        putCondition(mandatory, property, constraintRefGroups);
    }

    /**
     * Defines the property as immutable if at least one of the {@code constraintRefGroups} is true.<p/>
     * I.e. the AndGroups are ORed where the ConstrainRefs within each AndGroup are ANDed.<p/>
     * E.g. [Group.and(a, b), Group.and(c, d)] is evaluated as: a && b || c && d
     * 
     * @param property
     * @param constraintRefGroups
     */
    public void immutable(String property, AndGroup... constraintRefGroups) {
        putCondition(immutable, property, constraintRefGroups);
    }
    
    private void putCondition(Map<String, ConstraintRefGroups> conditionMap, String property, AndGroup... constraintRefGroups) {
        EasyValidator.validateProperty(property, typeClass);
        for (AndGroup andGroup : constraintRefGroups) {
            for (ConstraintRef ref : andGroup.getConstraintRefs()) {
                validatePropertyAndValueTypes(ref);
            }
        }
        putAndWarn(conditionMap, property, ConstraintRefGroups.ored(constraintRefGroups));
    }
    
    /**
     * Defines the property as mandatory if all of the {@code constraintRefGroups} are true.<p/>
     * I.e. the OrGroups are ANDed where the ConstrainRefs within each OrGroup are ORed.<p/>
     * E.g. [Group.or(e, f), Group.or(g, h)] is evaluated as: (e || f) && (g || h)
     * 
     * @param property
     * @param constraintRefGroups
     */
    public void mandatory(String property, OrGroup... constraintRefGroups) {
        putCondition(mandatory, property, constraintRefGroups);
    }

    /**
     * Defines the property as immutable if all of the {@code constraintRefGroups} are true.<p/>
     * I.e. the OrGroups are ANDed where the ConstrainRefs within each OrGroup are ORed.<p/>
     * E.g. [Group.or(e, f), Group.or(g, h)] is evaluated as: (e || f) && (g || h)
     * 
     * @param property
     * @param constraintRefGroups
     */
    public void immutable(String property, OrGroup... constraintRefGroups) {
        putCondition(immutable, property, constraintRefGroups);
    }
    
    private void putCondition(Map<String, ConstraintRefGroups> conditionMap, String property, OrGroup... constraintRefGroups) {
        EasyValidator.validateProperty(property, typeClass);
        for (OrGroup orGroup : constraintRefGroups) {
            for (ConstraintRef ref : orGroup.getConstraintRefs()) {
                validatePropertyAndValueTypes(ref);
            }
        }
        putAndWarn(conditionMap, property, ConstraintRefGroups.anded(constraintRefGroups));
    }
    
    /**
     * If the logical relation between your constraints are really complicated, this method may be your last resort ...
     * <p/>
     * This version defines the property as mandatory if the {@code groups} object evaluates to true.
     * <p/>
     * According to the logical operation the AndGroups and OrGroups are either ANDed or ORed.
     * <p/>
     * E.g. Groups.anded(Group.or(a, b), Group.or(c, d), Group.or(e, f)] is evaluated as: ...
     * 
     * @param property
     * @param groups
     */
    public void mandatory(String property, ConstraintRefGroups groups) {
        putCondition(mandatory, property, groups);
    }
    
    /**
     * If the logical relation between your constraints are really complicated, this method may be your last resort ...
     * <p/>
     * This version defines the property as immutable if the {@code groups} object evaluates to true.
     * <p/>
     * According to the logical operation the AndGroups and OrGroups are either ANDed or ORed.
     * <p/>
     * E.g. Groups.anded(Group.or(a, b), Group.or(c, d), Group.or(e, f)] is evaluated as: ...
     * 
     * @param property
     * @param groups
     */
    public void immutable(String property, ConstraintRefGroups groups) {
        putCondition(immutable, property, groups);
    }
    
    private void putCondition(Map<String, ConstraintRefGroups> conditionMap, String property, ConstraintRefGroups groups) {
        EasyValidator.validateProperty(property, typeClass);
        for (ConstraintRefGroup group : groups.getConstraintRefGroups()) {
            for (ConstraintRef ref : group.getConstraintRefs()) {
                validatePropertyAndValueTypes(ref);
            }
        }
        putAndWarn(conditionMap, property, groups);
    }
    
    
    private void putAndWarn(Map<String, ConstraintRefGroups> conditionMap, String property,
            ConstraintRefGroups constraintRefGroups) {
        if (conditionMap.containsKey(property)) {
            log.warn("Validation conditions for property '" + property + "' are already defined and will be overwritten.");
            conditionMap.remove(property); // ensure insertion order is 'overwritten' as well
        }
        put(conditionMap, property, constraintRefGroups);
    }

    private void put(Map<String, ConstraintRefGroups> conditionMap, String property, ConstraintRefGroups conditionGroups) {
        conditionMap.put(property, conditionGroups);
    }

    
    /**
     * Defines the constraint for the property content.
     * 
     * @param property
     * @param constraint
     */
    public void content(String property, Constraint constraint) {
        EasyValidator.validateProperty(property, typeClass);
        content.put(property, (ContentGroup) null);
    }

    /**
     * Defines the constraint for the property content if all {@code constraintRefs} are true.
     * I.e. the ConstraintRefs are ANDed. Convenience method for content(String, Constraint, AndGroup).
     * 
     * @param property
     * @param constraint
     * @param constraintRefs
     */
    public void content(String property, Constraint constraint, ConstraintRef... constraintRefs) {
        //TODO validate contentConstraint
        EasyValidator.validateProperty(property, typeClass);
        for (ConstraintRef ref : constraintRefs) {
            validatePropertyAndValueTypes(ref);
        }
        content.put(property,
                ContentGroup.ored(constraint, ConstraintRefGroups.anded(ConstraintRefGroup.and(constraintRefs))));
    }
    
    /**
     * TODO
     * 
     * @param property
     * @param constraint
     * @param andGroups
     */
    public void content(String property, Constraint constraint, AndGroup... andGroups) {
        //TODO validate contentConstraint 
        EasyValidator.validateProperty(property, typeClass);
        for (AndGroup andGroup : andGroups) {
            for (ConstraintRef ref : andGroup.getConstraintRefs()) {
                validatePropertyAndValueTypes(ref);
            }
        }
        // putAndWarn ...
    }

    /**
     * TODO
     * 
     * @param property
     * @param constraint
     * @param orGroups
     */
    public void content(String property, Constraint constraint, OrGroup... orGroups) {
        //TODO validate contentConstraint
        EasyValidator.validateProperty(property, typeClass);
        for (OrGroup andGroup : orGroups) {
            for (ConstraintRef ref : andGroup.getConstraintRefs()) {
                validatePropertyAndValueTypes(ref);
            }
        }
        // putAndWarn ...
    }
    
    /**
     * TODO
     * @param property
     * @param contentGroups
     */
    public void content(String property, ContentGroup... contentGroups) {
        EasyValidator.validateProperty(property, typeClass);
        for (ContentGroup contentGroup : contentGroups) {
            Constraint contentConstraint = contentGroup.getContentConstraint();
            //TODO validate contentConstraint ! e.g. if it's a EqualsAnyRef !! and because of matching types!
            // create a temp. a ConstraintRef ?!
            for (ConstraintRefGroup refGroup : contentGroup.getConstraintRefGroups().getConstraintRefGroups()) {
                for (ConstraintRef ref : refGroup.getConstraintRefs()) {
                    validatePropertyAndValueTypes(ref);
                }
            }
        }
        //content.put(property, contentGroups);
    }
    

    private void validatePropertyAndValueTypes(ConstraintRef constraintRef) {
        if (constraintRef == null) {
            throw new IllegalArgumentException("ConstraintRef null is not allowed");
        }
        String propertyName = constraintRef.getProperty();
        Class<?> propertyType = EasyValidator.validateProperty(propertyName, typeClass);
        Constraint constraint = constraintRef.getConstraint();
        
        // Check that constraint supports propertyType
        if (!constraint.isSupportedType(propertyType)) {
            throw new IllegalArgumentException(
                    "Contraint " + constraint.getClass().getSimpleName() + " does not support type of property "
                            + propertyName + " (" + propertyType + ")");
        }
        
        // Check arguments
        constraint.validateArgumentsOrFail(propertyType);
    }

    
    public Map<String, ConstraintRefGroups> getMandatory() {
        return mandatory;
    }

    public Map<String, ConstraintRefGroups> getImmutable() {
        return immutable;
    }

    public Map<String, ContentGroup> getContent() {
        return content;
    }

    protected Class<T> getTypeClass() {
        return typeClass;
    }

    /**
     * Overwrites the default key that is used as an identifier for the generic type {@code T} when this map is
     * serialized to JSON. The default key is the lowercase simple name of the class {@code T}.
     * 
     * @param typeJsonKey
     */
    public void setTypeJsonKey(String typeJsonKey) {
        this.typeJsonKey = typeJsonKey;
    }

    @Override
    public String serializeToJson() {
        String json = "{" + asKey(typeJsonKey) + "{";
        json += asKey("mandatory") + asObject(serializeConditions(mandatory)) + ",";
        json += asKey("immutable") + asObject(serializeConditions(immutable)) + ",";
        json += asKey("content") + asObject(serializeCondition(content));
        json += "}}";
        return json;
    }

    private String serializeConditions(Map<String, ConstraintRefGroups> conditionGroupsMap) {
        String json = "";
        boolean firstProp = true;
        for (String propertyKey : conditionGroupsMap.keySet()) {
            ConstraintRefGroups groups = conditionGroupsMap.get(propertyKey);
            String logicalOperator = groups.getLogicalOperator().name();
            json += (!firstProp ? "," : "") + asKey(propertyKey) + "{" + asKey("groupsOperator")
                    + quoted(logicalOperator) + "," + asKey("groups") + "[";
            firstProp = false;
            boolean firstGroup = true;
            for (ConstraintRefGroup group : groups.getConstraintRefGroups()) {
                json += (!firstGroup ? "," : "") + group.serializeToJson();
                firstGroup = false;
                group.serializeToJson();
            }
            if (groups.getConstraintRefGroups().length == 0) {
                json += "true";
            }
            json += "]}";
        }
        return json;
    }

    private String serializeCondition(Map<String, ContentGroup> content) {
        // TODO Auto-generated method stub
        return "TODO";
    }

}

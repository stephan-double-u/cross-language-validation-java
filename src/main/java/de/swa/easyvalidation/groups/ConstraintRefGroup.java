package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.constraints.ConstraintRef;
import de.swa.easyvalidation.constraints.Size;
import de.swa.easyvalidation.json.JsonSerializable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static de.swa.easyvalidation.json.JsonUtil.asArray;
import static de.swa.easyvalidation.json.JsonUtil.asKey;
import static de.swa.easyvalidation.json.JsonUtil.asObject;
import static de.swa.easyvalidation.json.JsonUtil.quoted;

public abstract class ConstraintRefGroup implements JsonSerializable {

    private static Logger log = LoggerFactory.getLogger(ConstraintRefGroup.class);

    public static final String ANDED_GROUP_JSON_VALUE = "AND";
    public static final String ORED_GROUP_JSON_VALUE = "OR";

    protected ConstraintRef[] constraintRefs;

    public static AndGroup and(final ConstraintRef... constraintRefs) {
        return new AndGroup(constraintRefs);
    }

    public static OrGroup or(final ConstraintRef... constraintRefs) {
        return new OrGroup(constraintRefs);
    }
    
    public ConstraintRef[] getConstraintRefs() {
        return constraintRefs;
    }
    
    @Override
    public String serializeToJson() {
        String groupValue = (this instanceof AndGroup) ? ANDED_GROUP_JSON_VALUE : ORED_GROUP_JSON_VALUE;
        return asObject(
                asKey("groupOperator") + quoted(groupValue) + "," + asKey("constraintRefs") + serializeGroupToArray());
    }
    
    public String serializeGroupToArray() {
        String json = "";
        boolean first = true;
        for (final ConstraintRef constraintRef : getConstraintRefs()) {
            json += (!first ? "," : "") + constraintRef.serializeToJson();
            first = false;
        }
        return asArray(json);
    }

    // TODO -> JUnit test
    public static void main(final String[] args) {
        log.debug(new AndGroup(Constraint.ref("someString", Size.minMax(1, 100)), Constraint.ref("articleList", Size.min(5))).serializeToJson());
        log.debug(new OrGroup(Constraint.ref("someString", Size.minMax(1, 100)), Constraint.ref("articleList", Size.min(5)) ).serializeToJson());
    }

}

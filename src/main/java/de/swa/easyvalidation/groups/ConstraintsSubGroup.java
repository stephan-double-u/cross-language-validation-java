package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.constraints.ConstraintRef;
import de.swa.easyvalidation.constraints.Size;
import de.swa.easyvalidation.json.JsonSerializable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.stream.Collectors;

import static de.swa.easyvalidation.json.JsonUtil.asArray;
import static de.swa.easyvalidation.json.JsonUtil.asKey;
import static de.swa.easyvalidation.json.JsonUtil.asObject;
import static de.swa.easyvalidation.json.JsonUtil.quoted;

public abstract class ConstraintsSubGroup implements JsonSerializable {

    private static Logger log = LoggerFactory.getLogger(ConstraintsSubGroup.class);

    protected ConstraintRef[] constraintRefs;

    public static final String ANDED_GROUP_JSON_VALUE = "AND";
    public static final String ORED_GROUP_JSON_VALUE = "OR";

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
        String operator = (this instanceof AndGroup) ? LogicalOperator.AND.name() : LogicalOperator.OR.name();
        final String refsAsJson = Arrays.stream(constraintRefs).map(r -> r.serializeToJson()).collect(Collectors.joining(","));
        return asObject(asKey("operator") + quoted(operator) + "," + asKey("constraints") + asArray(refsAsJson));
    }

}

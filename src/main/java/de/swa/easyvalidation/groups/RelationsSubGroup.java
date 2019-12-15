package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.PropConstraint;
import de.swa.easyvalidation.json.JsonSerializable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.stream.Collectors;

import static de.swa.easyvalidation.json.JsonUtil.asArray;
import static de.swa.easyvalidation.json.JsonUtil.asKey;
import static de.swa.easyvalidation.json.JsonUtil.asObject;
import static de.swa.easyvalidation.json.JsonUtil.quoted;

public abstract class RelationsSubGroup implements JsonSerializable {

    private static Logger log = LoggerFactory.getLogger(RelationsSubGroup.class);

    protected PropConstraint[] propConstraints;

    public static final String ANDED_GROUP_JSON_VALUE = "AND";
    public static final String ORED_GROUP_JSON_VALUE = "OR";

    public static AndGroup AND(final PropConstraint... propConstraints) {
        return new AndGroup(propConstraints);
    }

    public static OrGroup OR(final PropConstraint... propConstraints) {
        return new OrGroup(propConstraints);
    }

    public PropConstraint[] getPropConstraints() {
        return propConstraints;
    }

    @Override
    public String serializeToJson() {
        String operator = (this instanceof AndGroup) ? LogicalOperator.AND.name() : LogicalOperator.OR.name();
        final String refsAsJson = Arrays.stream(propConstraints).map(r -> r.serializeToJson()).collect(Collectors.joining(","));
        return asObject(asKey("operator") + quoted(operator) + "," + asKey("constraints") + asArray(refsAsJson));
    }

}

package de.swa.clv.groups;

import de.swa.clv.constraints.PropConstraint;
import de.swa.clv.json.JsonSerializable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.asArray;
import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.asObject;
import static de.swa.clv.json.JsonUtil.quoted;

public abstract class ConstraintsSubGroup implements JsonSerializable {

    private static Logger log = LoggerFactory.getLogger(ConstraintsSubGroup.class);

    protected PropConstraint[] propConstraints;

    public static final String ANDED_GROUP_JSON_VALUE = "AND";
    public static final String ORED_GROUP_JSON_VALUE = "OR";

    public static ConstraintsAndGroup AND(final PropConstraint... propConstraints) {
        return new ConstraintsAndGroup(propConstraints);
    }

    public static ConstraintsOrGroup OR(final PropConstraint... propConstraints) {
        return new ConstraintsOrGroup(propConstraints);
    }

    public PropConstraint[] getPropConstraints() {
        return propConstraints;
    }

    @Override
    public String serializeToJson() {
        String operator = (this instanceof ConstraintsAndGroup) ? LogicalOperator.AND.name() : LogicalOperator.OR.name();
        final String refsAsJson = Arrays.stream(propConstraints).map(r -> r.serializeToJson()).collect(Collectors.joining(","));
        return asObject(asKey("operator") + quoted(operator) + "," + asKey("constraints") + asArray(refsAsJson));
    }

}

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

public abstract class ConditionsGroup implements JsonSerializable {

    protected PropConstraint[] propConstraints;

    public static final String ANDED_GROUP_JSON_VALUE = "AND";
    public static final String ORED_GROUP_JSON_VALUE = "OR";

    public static ConditionsAndGroup AND(final PropConstraint... propConstraints) {
        return new ConditionsAndGroup(propConstraints);
    }

    public static ConditionsOrGroup OR(final PropConstraint... propConstraints) {
        return new ConditionsOrGroup(propConstraints);
    }

    public PropConstraint[] getPropConstraints() {
        return propConstraints;
    }

    @Override
    public String serializeToJson() {
        String operator = (this instanceof ConditionsAndGroup) ? LogicalOperator.AND.name() : LogicalOperator.OR.name();
        final String refsAsJson = Arrays.stream(propConstraints).map(PropConstraint::serializeToJson).collect(Collectors.joining(","));
        return asObject(asKey("operator") + quoted(operator) + "," + asKey("conditions") + asArray(refsAsJson));
    }

}

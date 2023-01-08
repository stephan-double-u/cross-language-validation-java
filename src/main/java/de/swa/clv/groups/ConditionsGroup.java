package de.swa.clv.groups;

import de.swa.clv.constraints.ConditionConstraint;
import de.swa.clv.json.JsonSerializable;

import java.util.Arrays;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.*;

@SuppressWarnings("squid:S100")
public abstract class ConditionsGroup implements JsonSerializable {

    protected ConditionConstraint[] constraints;

    public static ConditionsAndGroup AND(ConditionConstraint... constraints) {
        return new ConditionsAndGroup(constraints);
    }

    public static ConditionsOrGroup OR(ConditionConstraint... constraints) {
        return new ConditionsOrGroup(constraints);
    }

    public ConditionConstraint[] getConstraints() {
        return constraints;
    }

    @Override
    public String serializeToJson() {
        String operator = (this instanceof ConditionsAndGroup) ? LogicalOperator.AND.name() : LogicalOperator.OR.name();
        String refsAsJson = Arrays.stream(constraints)
                .map(ConditionConstraint::serializeToJson)
                .collect(Collectors.joining(","));
        return asObject(asKey("operator") + quoted(operator) + "," + asKey("conditions") + asArray(refsAsJson));
    }

}

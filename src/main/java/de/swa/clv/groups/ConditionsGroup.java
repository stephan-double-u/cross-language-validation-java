package de.swa.clv.groups;

import de.swa.clv.constraints.Condition;
import de.swa.clv.constraints.Constraint;
import de.swa.clv.json.JsonSerializable;

import java.util.Arrays;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.*;

@SuppressWarnings("squid:S100")
public abstract class ConditionsGroup implements JsonSerializable {

    protected Condition[] conditions;

    public static ConditionsAndGroup AND(Condition... conditions) {
        Constraint.assertNotNull(conditions);
        return new ConditionsAndGroup(conditions);
    }

    public static ConditionsOrGroup OR(Condition... conditions) {
        Constraint.assertNotNull(conditions);
        return new ConditionsOrGroup(conditions);
    }

    public Condition[] getConditions() {
        return conditions;
    }

    @Override
    public String serializeToJson() {
        String operator = (this instanceof ConditionsAndGroup) ? LogicalOperator.AND.name() : LogicalOperator.OR.name();
        String refsAsJson = Arrays.stream(conditions)
                .map(Condition::serializeToJson)
                .collect(Collectors.joining(","));
        return asObject(asKey("operator") + quoted(operator) + "," + asKey("conditions") + asArray(refsAsJson));
    }

}

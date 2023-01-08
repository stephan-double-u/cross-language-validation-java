package de.swa.clv.constraints;

import static de.swa.clv.json.JsonUtil.*;

import de.swa.clv.json.JsonSerializable;

public record ConditionConstraint(String property, Constraint constraint) implements JsonSerializable {

    @Override
    public String serializeToJson() {
        return asObject(asKey("property") + quoted(property) + "," + asKey("constraint") + asObject(constraint.serializeToJson()));
    }


}

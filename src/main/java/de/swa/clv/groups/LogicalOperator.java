package de.swa.clv.groups;

import de.swa.clv.json.JsonSerializable;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.quoted;

public enum LogicalOperator implements JsonSerializable {
    AND, OR;

    @Override
    public String serializeToJson() {
        return asKey("operator") + quoted(this.name());
    }
}

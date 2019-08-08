package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.json.JsonSerializable;
import de.swa.easyvalidation.json.JsonUtil;

import static de.swa.easyvalidation.json.JsonUtil.asKey;
import static de.swa.easyvalidation.json.JsonUtil.quoted;

public enum LogicalOperator implements JsonSerializable {
    AND, OR;

    @Override
    public String serializeToJson() {
        return asKey("operator") + quoted(this.name());
    }
}

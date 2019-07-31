package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.json.JsonSerializable;
import de.swa.easyvalidation.json.JsonUtil;

public enum LogicalOperator implements JsonSerializable {
    AND, OR;

    @Override
    public String serializeToJson() {
        return JsonUtil.asKey("operator") + JsonUtil.quoted(this.name());
    }
}

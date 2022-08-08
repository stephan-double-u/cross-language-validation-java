package de.swa.clv;

import static de.swa.clv.json.JsonUtil.*;
import static de.swa.clv.json.JsonUtil.quoted;

public record ErrorCodeControl(UseType type, String code) {

    public static ErrorCodeControl of(UseType type, String code) {
        return new ErrorCodeControl(type, code);
    }

    public String serializeToJson() {
        return asKey("errorCodeControl") + asObject(asKey("useType") + quoted(type.name()) + ","
                + asKey("code") + quoted(code));
    }
}

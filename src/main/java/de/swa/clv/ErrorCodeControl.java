package de.swa.clv;

import static de.swa.clv.json.JsonUtil.*;
import static de.swa.clv.json.JsonUtil.quoted;

public class ErrorCodeControl {

    private final UseType type;
    private final String code;

    private ErrorCodeControl(UseType type, String code) {
        this.type = type;
        this.code = code;
    }

    public static ErrorCodeControl of(UseType type, String code) {
        return new ErrorCodeControl(type, code);
    }

    public UseType getType() {
        return type;
    }

    public String getCode() {
        return code;
    }

    public String serializeToJson() {
        return asKey("errorCodeControl") + asObject(asKey("useType") + quoted(getType().name()) + ","
                + asKey("code") + quoted(code));
    }
}

package de.swa.clv.constraints;

import java.io.Serializable;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.quoted;

public class EqualsNotNull extends Equals {

    EqualsNotNull() {
    }

    @Override
    public String getToken() {
        return "EQUALS_NOT_NULL";
    }

    /**
     * 'Not null' check should be possible for all objects that are at least serializable.
     */
    @Override
    public boolean isSupportedType(Class<?> clazz) {
        return super.isSupportedType(clazz) || clazz instanceof Serializable;
    }

    @Override
    public boolean validate(final Object object, final Object ignored) {
        return object != null;
    }

    @Override
    public String serializeToJson() {
        return asKey("type") + quoted(getToken());
    }

}

package de.swa.clv.constraints;

import java.io.Serializable;

import static de.swa.clv.json.JsonUtil.*;

public class EqualsNull extends Equals {

    EqualsNull() {
    }

    @Override
    public String getType() {
        return "EQUALS_NULL";
    }

    /**
     * 'Null' check should be possible for all objects that are at least serializable.
     */
    @Override
    public boolean isSupportedType(Class<?> clazz) {
        return super.isSupportedType(clazz) || clazz instanceof Serializable;
    }

    @Override
    public boolean validate(final Object object, final Object ignored) {
        return object == null;
    }

    @Override
    public String serializeToJson() {
        return asKey("type") + quoted(getType());
    }

}

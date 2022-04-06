package de.swa.clv.constraints;

import java.io.Serializable;

public class EqualsNull extends EqualsRoot {

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
        return clazz instanceof Serializable;
    }

    @Override
    public boolean validate(final Object object, final Object ignored) {
        return object == null;
    }

}

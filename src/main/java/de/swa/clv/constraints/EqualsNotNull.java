package de.swa.clv.constraints;

import java.io.Serializable;

public class EqualsNotNull extends EqualsRoot {

    EqualsNotNull() {
    }

    @Override
    public String getType() {
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

}

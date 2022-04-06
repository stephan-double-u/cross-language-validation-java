package de.swa.clv.constraints;

import de.swa.clv.util.TypeHelper;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.Date;

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
        return clazz instanceof Serializable;
    }

    @Override
    public boolean validate(final Object object, final Object ignored) {
        return object != null;
    }

}

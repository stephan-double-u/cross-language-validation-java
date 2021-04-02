package de.swa.clv.constraints;

public class EqualsNotNull extends EqualsRoot {

    EqualsNotNull() {
    }

    @Override
    public String getType() {
        return "EQUALS_NOT_NULL";
    }

    @Override
    public boolean validate(final Object object, final Object ignored) {
        return object != null;
    }

}

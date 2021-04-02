package de.swa.clv.constraints;

public class EqualsNull extends EqualsRoot {

    EqualsNull() {
    }

    @Override
    public String getType() {
        return "EQUALS_NULL";
    }

    @Override
    public boolean validate(final Object object, final Object ignored) {
        return object == null;
    }

}

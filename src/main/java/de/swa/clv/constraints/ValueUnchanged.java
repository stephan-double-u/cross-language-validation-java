package de.swa.clv.constraints;

import java.util.Objects;

class ValueUnchanged extends ValueConstraint implements IsUpdateConstraint {

    @Override
    public String getToken() {
        return Value.TOKEN.VALUE_UNCHANGED.name();
    }

    @Override
    public boolean validate(Object thisValue, Object thatValue) {
        return Objects.deepEquals(thisValue, thatValue);
    }
}

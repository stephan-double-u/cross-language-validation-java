package de.swa.clv.constraints;

import java.util.Objects;

class ValueChanged extends ValueConstraint implements IsUpdateConstraint {

    @Override
    public String getToken() {
        return Value.TOKEN.VALUE_CHANGED.name();
    }

    @Override
    public boolean validate(Object thisValue, Object thatValue) {
        return !Objects.deepEquals(thisValue, thatValue);
    }

}

package de.swa.clv.constraints;

abstract class EqualsRef extends Equals {

    private boolean ofUpdate = false;
    private boolean ofCurrent = false;

    @Override
    public boolean isSupportedType(Class<?> clazz) {
        return true;
    }

    public boolean validateReferencedValue(Object objectToValidate, Object value) {
        return Equals.equalsUntyped(objectToValidate, value);
    }

    public boolean isOfUpdate() {
        return ofUpdate;
    }

    public void setOfUpdate(boolean ofUpdate) {
        this.ofUpdate = ofUpdate;
    }

    public boolean isOfCurrent() {
        return ofCurrent;
    }

    public void setOfCurrent(boolean ofCurrent) {
        this.ofCurrent = ofCurrent;
    }
}

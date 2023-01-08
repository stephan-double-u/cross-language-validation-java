package de.swa.clv.constraints;

public abstract class Value extends Constraint {

    public enum TOKEN {
        VALUE_CHANGED, VALUE_UNCHANGED
    }

    public static ValueChanged changed() {
        return new ValueChanged();
    }

    public static ValueUnchanged unchanged() {
        return new ValueUnchanged();
    }

}

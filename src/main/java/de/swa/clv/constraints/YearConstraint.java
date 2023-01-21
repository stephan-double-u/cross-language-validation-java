package de.swa.clv.constraints;

public abstract class YearConstraint extends Dates {

    @Override
    public String serializeToJson() {
        return serializeToJsonAsValuesArray();
    }

}

package de.swa.clv.constraints;

public abstract class QuarterConstraint extends Dates {

    @Override
    public String serializeToJson() {
        return serializeToJsonAsValuesArray();
    }

}

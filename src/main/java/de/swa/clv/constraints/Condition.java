package de.swa.clv.constraints;

public interface Condition {

    static ConditionConstraint of(final String propertyName, final Constraint constraint) {
        return new ConditionConstraint(propertyName, constraint);
    }

}

package de.swa.clv.constraints;

public interface Condition {

    static PropConstraint of(final String propertyName, final Constraint constraint) {
        return new PropConstraint(propertyName, constraint);
    }

}

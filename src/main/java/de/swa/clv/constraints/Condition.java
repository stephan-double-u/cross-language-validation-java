package de.swa.clv.constraints;

public interface Condition {

    static PropConstraint of(final String propertyName, final ConstraintRoot constraint) {
        return new PropConstraint(propertyName, constraint);
    }

}

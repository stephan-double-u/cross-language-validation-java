package de.swa.easyvalidation.constraints;

public abstract class Constraint {

    private Constraint() {
    }

    public static PropConstraint ref(final String propertyName, final ConstraintRoot constraint) {
        return new PropConstraint(propertyName, constraint);
    }

}

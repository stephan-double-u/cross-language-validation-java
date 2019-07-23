package de.swa.easyvalidation.constraints;

public abstract class Constraint extends ConstraintRoot {

    public static ConstraintRef ref(final String propertyName, final Constraint constraint) {
        return new ConstraintRef(propertyName, constraint);
    }

}

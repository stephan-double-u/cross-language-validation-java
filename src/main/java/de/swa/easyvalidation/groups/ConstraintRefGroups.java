package de.swa.easyvalidation.groups;

public class ConstraintRefGroups {

    public static enum Logical {
        AND, OR
    };

    private Logical logicalOperator;
    private ConstraintRefGroup[] constraintRefGroups;

    private ConstraintRefGroups(Logical logicalOperator, ConstraintRefGroup... constraintRefGroups) {
        this.logicalOperator = logicalOperator;
        this.constraintRefGroups = constraintRefGroups;
    }

    public static ConstraintRefGroups anded(ConstraintRefGroup... constraintRefGroups) {
        return new ConstraintRefGroups(Logical.AND, constraintRefGroups);
    }

    public static ConstraintRefGroups ored(ConstraintRefGroup... constraintRefGroups) {
        return new ConstraintRefGroups(Logical.OR, constraintRefGroups);
    }

    public Logical getLogicalOperator() {
        return logicalOperator;
    }

    public ConstraintRefGroup[] getConstraintRefGroups() {
        return constraintRefGroups;
    }

}

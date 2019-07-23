package de.swa.easyvalidation.groups;

public class ConstraintRefTopGroup {

    public static enum Logical {
        AND, OR
    };

    private final Logical logicalOperator;
    private final ConstraintRefGroup[] constraintRefGroups;

    private ConstraintRefTopGroup(final Logical logicalOperator, final ConstraintRefGroup... constraintRefGroups) {
        this.logicalOperator = logicalOperator;
        this.constraintRefGroups = constraintRefGroups;
    }

    public static ConstraintRefTopGroup anded(final ConstraintRefGroup... constraintRefGroups) {
        return new ConstraintRefTopGroup(Logical.AND, constraintRefGroups);
    }

    public static ConstraintRefTopGroup ored(final ConstraintRefGroup... constraintRefGroups) {
        return new ConstraintRefTopGroup(Logical.OR, constraintRefGroups);
    }

    public Logical getLogicalOperator() {
        return logicalOperator;
    }

    public ConstraintRefGroup[] getConstraintRefGroups() {
        return constraintRefGroups;
    }

}

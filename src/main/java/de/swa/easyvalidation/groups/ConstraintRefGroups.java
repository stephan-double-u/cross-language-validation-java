package de.swa.easyvalidation.groups;

public class ConstraintRefGroups {

    private LogicalOperator logicalOperator;
    private ConstraintRefGroup[] constraintRefGroups;

    private ConstraintRefGroups(LogicalOperator logicalOperator, ConstraintRefGroup... constraintRefGroups) {
        this.logicalOperator = logicalOperator;
        this.constraintRefGroups = constraintRefGroups;
    }

    public static ConstraintRefGroups anded(ConstraintRefGroup... constraintRefGroups) {
        return new ConstraintRefGroups(LogicalOperator.AND, constraintRefGroups);
    }

    public static ConstraintRefGroups ored(ConstraintRefGroup... constraintRefGroups) {
        return new ConstraintRefGroups(LogicalOperator.OR, constraintRefGroups);
    }

    public LogicalOperator getLogicalOperator() {
        return logicalOperator;
    }

    public ConstraintRefGroup[] getConstraintRefGroups() {
        return constraintRefGroups;
    }

}

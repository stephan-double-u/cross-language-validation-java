package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.json.JsonSerializable;
import de.swa.easyvalidation.json.JsonUtil;

import java.util.Arrays;
import java.util.stream.Collectors;

import static de.swa.easyvalidation.json.JsonUtil.*;

public class ConstraintRefTopGroup implements JsonSerializable {

    private final LogicalOperator logicalOperator;

    private final ConstraintRefGroup[] constraintRefGroups;

    private ConstraintRefTopGroup(final LogicalOperator logicalOperator, final ConstraintRefGroup... constraintRefGroups) {
        this.logicalOperator = logicalOperator;
        this.constraintRefGroups = constraintRefGroups;
    }

    public static ConstraintRefTopGroup anded(final ConstraintRefGroup... constraintRefGroups) {
        return new ConstraintRefTopGroup(LogicalOperator.AND, constraintRefGroups);
    }

    public static ConstraintRefTopGroup ored(final ConstraintRefGroup... constraintRefGroups) {
        return new ConstraintRefTopGroup(LogicalOperator.OR, constraintRefGroups);
    }

    public LogicalOperator getLogicalOperator() {
        return logicalOperator;
    }

    public ConstraintRefGroup[] getConstraintRefGroups() {
        return constraintRefGroups;
    }

    @Override
    public String serializeToJson() {
        if (constraintRefGroups.length == 0) {
            return "";
        }
        final String groupsAsJson = Arrays.stream(constraintRefGroups).map(g -> g.serializeToJson()).collect(Collectors.joining(","));
        return asKey("contraintsTopGroup") + asObject(logicalOperator.serializeToJson() + "," + asKey("constraintsSubGroups") + asArray(groupsAsJson));
    }

}


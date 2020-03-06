package de.swa.clv.groups;

import de.swa.clv.json.JsonSerializable;

import java.util.Arrays;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.*;

public class ConstraintsTopGroup implements JsonSerializable {

    private final LogicalOperator logicalOperator;
    private final ConstraintsSubGroup[] constraintsSubGroups;

    private ConstraintsTopGroup(final LogicalOperator logicalOperator, final ConstraintsSubGroup... constraintsSubGroups) {
        this.logicalOperator = logicalOperator;
        this.constraintsSubGroups = constraintsSubGroups;
    }

    public static ConstraintsTopGroup AND(final ConstraintsSubGroup... constraintsSubGroups) {
        return new ConstraintsTopGroup(LogicalOperator.AND, constraintsSubGroups);
    }

    public static ConstraintsTopGroup OR(final ConstraintsSubGroup... constraintsSubGroups) {
        return new ConstraintsTopGroup(LogicalOperator.OR, constraintsSubGroups);
    }

    public LogicalOperator getLogicalOperator() {
        return logicalOperator;
    }

    public ConstraintsSubGroup[] getConstraintsSubGroups() {
        return constraintsSubGroups;
    }

    @Override
    public String serializeToJson() {
        if (constraintsSubGroups.length == 0) {
            return "";
        }
        final String groupsAsJson = Arrays.stream(constraintsSubGroups).map(g -> g.serializeToJson()).collect(Collectors.joining(","));
        return asKey("constraintsTopGroup") + asObject(logicalOperator.serializeToJson() + "," + asKey("constraintsSubGroups") + asArray(groupsAsJson));
    }

}


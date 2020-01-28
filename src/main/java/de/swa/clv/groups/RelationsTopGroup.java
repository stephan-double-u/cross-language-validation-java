package de.swa.clv.groups;

import de.swa.clv.json.JsonSerializable;

import java.util.Arrays;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.*;

public class RelationsTopGroup implements JsonSerializable {

    private final LogicalOperator logicalOperator;
    private final RelationsSubGroup[] relationsSubGroups;

    private RelationsTopGroup(final LogicalOperator logicalOperator, final RelationsSubGroup... relationsSubGroups) {
        this.logicalOperator = logicalOperator;
        this.relationsSubGroups = relationsSubGroups;
    }

    public static RelationsTopGroup AND(final RelationsSubGroup... relationsSubGroups) {
        return new RelationsTopGroup(LogicalOperator.AND, relationsSubGroups);
    }

    public static RelationsTopGroup OR(final RelationsSubGroup... relationsSubGroups) {
        return new RelationsTopGroup(LogicalOperator.OR, relationsSubGroups);
    }

    public LogicalOperator getLogicalOperator() {
        return logicalOperator;
    }

    public RelationsSubGroup[] getRelationsSubGroups() {
        return relationsSubGroups;
    }

    @Override
    public String serializeToJson() {
        if (relationsSubGroups.length == 0) {
            return "";
        }
        final String groupsAsJson = Arrays.stream(relationsSubGroups).map(g -> g.serializeToJson()).collect(Collectors.joining(","));
        return asKey("relationsTopGroup") + asObject(logicalOperator.serializeToJson() + "," + asKey("relationsSubGroups") + asArray(groupsAsJson));
    }

}


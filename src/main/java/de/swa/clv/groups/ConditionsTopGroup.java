package de.swa.clv.groups;

import de.swa.clv.constraints.Constraint;
import de.swa.clv.json.JsonSerializable;

import java.util.Arrays;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.*;

@SuppressWarnings("squid:S100")
public class ConditionsTopGroup implements JsonSerializable {

    private final LogicalOperator logicalOperator;
    private final ConditionsGroup[] conditionsGroups;

    protected ConditionsTopGroup(LogicalOperator logicalOperator, ConditionsGroup... conditionsGroups) {
        this.logicalOperator = logicalOperator;
        this.conditionsGroups = conditionsGroups;
    }

    public static ConditionsTopGroup AND(ConditionsGroup... conditionsGroups) {
        Constraint.assertNotNull(conditionsGroups);
        return new ConditionsTopGroup(LogicalOperator.AND, conditionsGroups);
    }

    public static ConditionsTopGroup OR(ConditionsGroup... conditionsGroups) {
        Constraint.assertNotNull(conditionsGroups);
        return new ConditionsTopGroup(LogicalOperator.OR, conditionsGroups);
    }

    public LogicalOperator getLogicalOperator() {
        return logicalOperator;
    }

    public ConditionsGroup[] getConditionsGroups() {
        return conditionsGroups;
    }

    @Override
    public String serializeToJson() {
        if (conditionsGroups.length == 0) {
            return "";
        }
        if (conditionsGroups.length == 1) {
            ConditionsGroup singleSubGroup = conditionsGroups[0];
            if (singleSubGroup.conditions.length == 1) {
                return asKey("condition") + singleSubGroup.conditions[0].serializeToJson();
            } else {
                return asKey("conditionsGroup") + singleSubGroup.serializeToJson();
            }
        } else {
            final String subGroupsAsJson = Arrays.stream(conditionsGroups)
                    .map(ConditionsGroup::serializeToJson)
                    .collect(Collectors.joining(","));
            return asKey("conditionsTopGroup") + asObject(logicalOperator.serializeToJson() + "," +
                    asKey("conditionsGroups") + asArray(subGroupsAsJson));
        }
    }

}


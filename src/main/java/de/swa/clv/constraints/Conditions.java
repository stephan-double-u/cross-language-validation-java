package de.swa.clv.constraints;

import de.swa.clv.ValidationRules;
import de.swa.clv.groups.ConditionsTopGroup;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.asObject;

public class Conditions {

    private final ConstraintRoot constraint;
    private final Permissions permissions;
    private final ConditionsTopGroup conditionsTopGroup;

    public Conditions(final ConstraintRoot constraint, Permissions permissions,
                      final ConditionsTopGroup conditionsTopGroup) {
        this.constraint = constraint;
        this.permissions = permissions;
        this.conditionsTopGroup = conditionsTopGroup;
    }

    public ConstraintRoot getConstraint() {
        return constraint;
    }

    public Permissions getPermissions() {
        return permissions;
    }

    public ConditionsTopGroup getConditionsTopGroup() {
        return conditionsTopGroup;
    }

    public static String serializeToJson(Conditions conditions) {
        final String constraintJson = ValidationRules.NO_CONSTRAINT == conditions.getConstraint() ? "" :
                asKey("constraint") + asObject(conditions.getConstraint().serializeToJson());
        final String permissionsJson = conditions.getPermissions().serializeToJson();
        final String topGroupJson = conditions.getConditionsTopGroup().serializeToJson();
        final String sep1 = "".equals(constraintJson) || "".equals(permissionsJson) ? "" : ",";
        final String sep2 = "".equals(topGroupJson) ? "" : ",";
        String conditionsJson = asObject(constraintJson + sep1 + permissionsJson + sep2 + topGroupJson);
        return "{}".equals(conditionsJson) ? "" : conditionsJson;
    }

}

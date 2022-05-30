package de.swa.clv.constraints;

import de.swa.clv.ErrorCodeControl;
import de.swa.clv.ValidationRules;
import de.swa.clv.groups.ConditionsTopGroup;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.asObject;

public class Conditions {

    private final ConstraintRoot constraint;
    private final Permissions permissions;
    private final ConditionsTopGroup conditionsTopGroup;
    private final ErrorCodeControl errorCodeControl;

    public Conditions(ConstraintRoot constraint, Permissions permissions, ConditionsTopGroup conditionsTopGroup,
            ErrorCodeControl errorCodeControl) {
        this.constraint = constraint;
        this.permissions = permissions;
        this.conditionsTopGroup = conditionsTopGroup;
        this.errorCodeControl = errorCodeControl;
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

    public ErrorCodeControl getErrorCodeControl() {
        return errorCodeControl;
    }

    public static String serializeToJson(Conditions conditions) {
        final String constraintJson = ValidationRules.NO_CONSTRAINT == conditions.getConstraint() ? "" :
                asKey("constraint") + asObject(conditions.getConstraint().serializeToJson());
        final String permissionsJson = conditions.getPermissions().serializeToJson();
        final String topGroupJson = conditions.getConditionsTopGroup().serializeToJson();
        ErrorCodeControl errorCodeControl = conditions.getErrorCodeControl();
        final String errorCodeJson = errorCodeControl != null ? errorCodeControl.serializeToJson() : "";
        boolean noConstraint = constraintJson.isEmpty();
        boolean noPermissions = permissionsJson.isEmpty();
        boolean noConditions = topGroupJson.isEmpty();
        final String sep1 = noConstraint || noPermissions ? "" : ",";
        final String sep2 = noConstraint && noPermissions ? "" : noConditions ? "" : ",";
        final String errorCodeJsonPart = errorCodeJson.isEmpty() ? "" : "," + errorCodeJson;
        return asObject(constraintJson + sep1 + permissionsJson + sep2 + topGroupJson + errorCodeJsonPart);
    }

}

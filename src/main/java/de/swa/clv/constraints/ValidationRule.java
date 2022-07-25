package de.swa.clv.constraints;

import de.swa.clv.ErrorCodeControl;
import de.swa.clv.UseType;
import de.swa.clv.ValidationRules;
import de.swa.clv.groups.ConditionsTopGroup;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.asObject;

public class ValidationRule {

    private String property;
    private final ConstraintRoot constraint;
    private final Permissions permissions;
    private final ConditionsTopGroup conditionsTopGroup;
    private ErrorCodeControl errorCodeControl;
    private boolean doNotSerialize = false;

    public ValidationRule(String property, ConstraintRoot constraint, Permissions permissions,
            ConditionsTopGroup conditionsTopGroup) {
        this.property = property;
        this.constraint = constraint;
        this.permissions = permissions;
        this.conditionsTopGroup = conditionsTopGroup;
    }

    public String getProperty() {
        return property;
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

    public ValidationRule errorCodeControl(UseType type, String code) {
        errorCodeControl = ErrorCodeControl.of(type, code);
        return this;
    }

    public boolean isDoNotSerialize() {
        return doNotSerialize;
    }

    public ValidationRule doNotSerialize() {
        doNotSerialize = true;
        return this;
    }

    public String serializeToJson() {
        if (doNotSerialize) {
            return "";
        }
        final String constraintJson = ValidationRules.NO_CONSTRAINT == getConstraint() ? "" :
                asKey("constraint") + asObject(getConstraint().serializeToJson());
        final String permissionsJson = getPermissions().serializeToJson();
        final String topGroupJson = getConditionsTopGroup().serializeToJson();
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

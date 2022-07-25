package de.swa.clv.constraints;

import de.swa.clv.ErrorCodeControl;
import de.swa.clv.UseType;
import de.swa.clv.ValidationRules;
import de.swa.clv.groups.ConditionsGroup;
import de.swa.clv.groups.ConditionsTopGroup;
import de.swa.clv.test.Util;
import org.junit.Test;

import static org.junit.Assert.*;

public class ValidationRuleTest {

    ConstraintRoot someConstraint = Equals.any("FOO");
    Permissions somePermissions = Permissions.all("BAR");
    ConditionsTopGroup someTopGroup = ConditionsTopGroup.AND(
            ConditionsGroup.OR(Condition.of("prop", Equals.none("ZOO")))
    );
    ErrorCodeControl errorCodeControl = ErrorCodeControl.of(UseType.AS_SUFFIX, "#suffix");
    String someConstraintJson = "'constraint':{'type':'EQUALS_ANY','values':['FOO']}";
    String somePermissionsJson = "'permissions':{'type':'ALL','values':['BAR']}";
    String someTopGroupJson = "'condition':{'property':'prop','constraint':{'type':'EQUALS_NONE','values':['ZOO']}}";
    String errorCodeControlJson = "'errorCodeControl':{'useType':'AS_SUFFIX','code':'#suffix'}";

    @Test
    public void serializeToJson_constraintAndPermissionsAndConditionsAndErrorCodeControl() {
        ValidationRule validationRule = new ValidationRule("anyProp", someConstraint, somePermissions, someTopGroup)
                .errorCodeControl(UseType.AS_SUFFIX, "#suffix");

        String json = validationRule.serializeToJson();

        String expected = Util.doubleQuote("{" + someConstraintJson + "," + somePermissionsJson + "," +
                someTopGroupJson + "," + errorCodeControlJson + "}");
        assertEquals(expected, json);
    }

    @Test
    public void serializeToJson_NoConstraint() {
        ValidationRule validationRule = new ValidationRule("anyProp", ValidationRules.NO_CONSTRAINT, somePermissions,
                someTopGroup);

        String json = validationRule.serializeToJson();

        String expected = Util.doubleQuote("{" + somePermissionsJson + "," + someTopGroupJson + "}");
        assertEquals(expected, json);
    }

    @Test
    public void serializeToJson_NoPermissions() {
        ValidationRule validationRule = new ValidationRule("anyProp", someConstraint, ValidationRules.NO_PERMISSIONS,
                someTopGroup);

        String json = validationRule.serializeToJson();

        String expected = Util.doubleQuote("{" + someConstraintJson + "," + someTopGroupJson + "}");
        assertEquals(expected, json);
    }

    @Test
    public void serializeToJson_NoConditions() {
        ValidationRule validationRule = new ValidationRule("anyProp", someConstraint, somePermissions,
                ValidationRules.NO_CONDITIONS_TOP_GROUP);

        String json = validationRule.serializeToJson();

        String expected = Util.doubleQuote("{" + someConstraintJson + "," + somePermissionsJson + "}");
        assertEquals(expected, json);
    }

    @Test
    public void serializeToJson_constraintOnly() {
        ValidationRule validationRule = new ValidationRule("anyProp", someConstraint, ValidationRules.NO_PERMISSIONS,
                ValidationRules.NO_CONDITIONS_TOP_GROUP);

        String json = validationRule.serializeToJson();

        String expected = Util.doubleQuote("{" + someConstraintJson + "}");
        assertEquals(expected, json);
    }

    @Test
    public void serializeToJson_permissionsOnly() {
        ValidationRule validationRule = new ValidationRule("anyProp", ValidationRules.NO_CONSTRAINT, somePermissions,
                ValidationRules.NO_CONDITIONS_TOP_GROUP);

        String json = validationRule.serializeToJson();

        String expected = Util.doubleQuote("{" + somePermissionsJson + "}");
        assertEquals(expected, json);
    }

    @Test
    public void serializeToJson_conditionsOnly() {
        ValidationRule validationRule = new ValidationRule("anyProp", ValidationRules.NO_CONSTRAINT,
                ValidationRules.NO_PERMISSIONS, someTopGroup);

        String json = validationRule.serializeToJson();

        String expected = Util.doubleQuote("{" + someTopGroupJson + "}");
        assertEquals(expected, json);
    }

    @Test
    public void serializeToJson_doNotSerialize() {
        ValidationRule validationRule = new ValidationRule("anyProp", someConstraint, somePermissions, someTopGroup)
                .errorCodeControl(UseType.AS_SUFFIX, "#suffix")
                .doNotSerialize();
        String json = validationRule.serializeToJson();

        assertEquals("", json);
    }


}

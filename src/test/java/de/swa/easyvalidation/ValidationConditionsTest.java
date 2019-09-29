package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.constraints.Equals;
import de.swa.easyvalidation.constraints.Permissions;
import de.swa.easyvalidation.constraints.Size;
import de.swa.easyvalidation.test.Util;
import org.hamcrest.core.StringContains;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.List;

import static org.junit.Assert.*;

public class ValidationConditionsTest {

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Test
    public void exceptionIfMandatoryPropertyWithoutPermissionsIsAlreadyDefined() {
        ValidationConditions<ClassOne> conditions = new ValidationConditions<>(ClassOne.class);
        conditions.mandatory("stringProp");

        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("conditions for property 'stringProp' (w/o permissions) are already defined"));

        conditions.mandatory("stringProp", Constraint.ref("stringProp", Size.minMax(1, 100)));
    }

    @Test
    public void exceptionIfMandatoryPropertyForAnyPermissionIsAlreadyDefined() {
        ValidationConditions<ClassOne> conditions = new ValidationConditions<>(ClassOne.class);
        conditions.mandatory("stringProp");
        conditions.mandatory("stringProp", Permissions.any("PERM1"));
        conditions.mandatory("stringProp", Permissions.any("PERM2"));

        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("conditions for property 'stringProp' and permission 'PERM2' are already defined"));

        conditions.mandatory("stringProp", Permissions.any(Perms.PERM1, Perms.PERM2));
    }

    @Test
    public void exceptionIfPropertyWithIndexDefinitionHasWrongType() {
        ValidationConditions<ClassOne> conditions = new ValidationConditions<>(ClassOne.class);
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Index definitions are only allowed for properties of type List or arrays: stringProp[0]"));
        conditions.mandatory("stringProp[0]");
    }

    @Test
    public void serializeEmptyConditionsInstance() {
        ValidationConditions<ClassOne> conditions = new ValidationConditions<>(ClassOne.class);
        final String jsonResult = conditions.serializeToJson();
        final String expected = Util.doubleQuote("{'schema-version':'0.1','mandatoryConditions':{},'immutableConditions':{},'contentConditions':{}}");
        assertEquals(expected, jsonResult);
    }

    @Test
    public void serializeTwoEmptyConditions() {
        ValidationConditions<ClassOne> cond1 = new ValidationConditions<>(ClassOne.class);
        ValidationConditions<ClassTwo> cond2 = new ValidationConditions<>(ClassTwo.class);
        final String jsonResult = ValidationConditions.serializeToJson(cond1, cond2);
        final String expected = Util.doubleQuote("{'schema-version':'0.1','mandatoryConditions':{},'immutableConditions':{},'contentConditions':{}}");
        assertEquals(expected, jsonResult);
    }

    @Test
    public void serializeTwoConditions() {
        ValidationConditions<ClassOne> cond1 = new ValidationConditions<>(ClassOne.class);
        cond1.mandatory("stringArrayProp");
        cond1.immutable("stringProp");
        ValidationConditions<ClassTwo> cond2 = new ValidationConditions<>(ClassTwo.class);
        cond2.immutable("stringProp");
        cond2.content("stringProp", Equals.any("Foo"));
        String jsonResult = ValidationConditions.serializeToJson(cond1, cond2);
        final String expected = Util.doubleQuote("{'schema-version':'0.1'," +
                "'mandatoryConditions':{'classone':{'stringArrayProp':[{}]}}," +
                "'immutableConditions':{'classone':{'stringProp':[{}]},'classtwo':{'stringProp':[{}]}}," +
                "'contentConditions':{'classtwo':{'stringProp':[{'contentConstraint':{'type':'EQUALS_ANY','values':['Foo']}}]}}}");
        assertEquals(expected, jsonResult);

    }


    @Test
    public void temp() {
        ValidationConditions<ClassOne> conditions = new ValidationConditions<>(ClassOne.class);
        conditions.mandatory("stringArrayProp[1-3]", Constraint.ref("stringArrayProp[4]", Equals.anyRef("stringArrayProp[5,6]")));
        assertTrue(true);
    }

    @Test
    public void manyManyMore() {
        assertTrue(true);
    }

    class ClassOne {
        private String stringProp;
        private String[] stringArrayProp;
        private List<String> stringListProp;
        public String getStringProp() {
            return stringProp;
        }
        public String[] getStringArrayProp() {
            return stringArrayProp;
        }
        public List<String> getStringListProp() {
            return stringListProp;
        }
    }

    class ClassTwo {
        private String stringProp;
        public String getStringProp() {
            return stringProp;
        }
    }

    enum Perms {
        PERM1, PERM2
    }
}
package de.swa.clv;

import de.swa.clv.constraints.Constraint;
import de.swa.clv.constraints.Equals;
import de.swa.clv.constraints.Permissions;
import de.swa.clv.constraints.Size;
import de.swa.clv.test.Util;
import org.hamcrest.core.StringContains;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.List;

import static org.junit.Assert.*;

public class ValidationRulesTest {

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Test
    public void exceptionIfMandatoryPropertyWithoutPermissionsIsAlreadyDefined() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        rules.mandatory("stringProp");

        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("rules for property 'stringProp' (w/o permissions) are already defined"));

        rules.mandatory("stringProp", Constraint.ref("stringProp", Size.minMax(1, 100)));
    }

    @Test
    public void exceptionIfMandatoryPropertyForAnyPermissionIsAlreadyDefined() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        rules.mandatory("stringProp");
        rules.mandatory("stringProp", Permissions.any("PERM1"));
        rules.mandatory("stringProp", Permissions.any("PERM2"));

        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("rules for property 'stringProp' and permission 'PERM2' are already defined"));

        rules.mandatory("stringProp", Permissions.any(Perms.PERM1, Perms.PERM2));
    }

    @Test
    public void exceptionIfPropertyWithIndexDefinitionHasWrongType() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Index definitions are only allowed for properties of type List or arrays: stringProp[0]"));
        rules.mandatory("stringProp[0]");
    }

    @Test
    public void serializeEmptyRulesInstance() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        final String jsonResult = rules.serializeToJson();
        final String expected = Util.doubleQuote("{'schema-version':'0.1','mandatoryRules':{},'immutableRules':{},'contentRules':{}}");
        assertEquals(expected, jsonResult);
    }

    @Test
    public void serializeTwoEmptyRules() {
        ValidationRules<ClassOne> cond1 = new ValidationRules<>(ClassOne.class);
        ValidationRules<ClassTwo> cond2 = new ValidationRules<>(ClassTwo.class);
        final String jsonResult = ValidationRules.serializeToJson(cond1, cond2);
        final String expected = Util.doubleQuote("{'schema-version':'0.1','mandatoryRules':{},'immutableRules':{},'contentRules':{}}");
        assertEquals(expected, jsonResult);
    }

    @Test
    public void serializeTwoRules() {
        ValidationRules<ClassOne> cond1 = new ValidationRules<>(ClassOne.class);
        cond1.mandatory("stringArrayProp");
        cond1.immutable("stringProp");
        ValidationRules<ClassTwo> cond2 = new ValidationRules<>(ClassTwo.class);
        cond2.immutable("stringProp");
        cond2.content("stringProp", Equals.any("Foo"));
        String jsonResult = ValidationRules.serializeToJson(cond1, cond2);
        final String expected = Util.doubleQuote("{'schema-version':'0.1'," +
                "'mandatoryRules':{'classone':{'stringArrayProp':[]}}," +
                "'immutableRules':{'classone':{'stringProp':[]},'classtwo':{'stringProp':[]}}," +
                "'contentRules':{'classtwo':{'stringProp':[{'contentConstraint':{'type':'EQUALS_ANY','values':['Foo']}}]}}}");
        assertEquals(expected, jsonResult);

    }


    @Test
    public void temp() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        rules.mandatory("stringArrayProp[1-3]", Constraint.ref("stringArrayProp[4]", Equals.anyRef("stringArrayProp[5,6]")));
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
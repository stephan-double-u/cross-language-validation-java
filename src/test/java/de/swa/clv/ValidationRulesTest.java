package de.swa.clv;

import de.swa.clv.constraints.Condition;
import de.swa.clv.constraints.Equals;
import de.swa.clv.test.Util;
import org.hamcrest.core.StringContains;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert

.*;

public class ValidationRulesTest {

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Test
    public void exceptionIfPropertyHasWildcardTypeWithLowerBounds() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);

        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString(
                "Index definitions for generics with lower bounds wildcard type is not implemented (and quite useless(?)): supInteger[*]"));

        rules.mandatory("supInteger[*]");
    }

    @Test
    public void exceptionIfPropertyWithIndexDefinitionHasWrongType() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Index definitions are only allowed for properties of type List or arrays: stringProp[0]"));
        rules.mandatory("stringProp[0]");
    }

    @Test
    public void mandatoryIndexedPropertiesEverywhere() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        try {
            rules.mandatory("stringArrayProp[1-3]", Condition.of("stringArrayProp[4]", Equals.anyRef("stringArrayProp" +
                        "[5,6]")));
        } catch (Exception e) {
            fail();
        }
    }

    @Test
    public void mandatoryIndexedPropertyWildcardUpperBounds() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        try {
            rules.mandatory("extNumber[*]");
        } catch (Exception e) {
            fail();
        }
    }


    @Test
    public void serializeEmptyRulesInstance() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        final String jsonResult = rules.serializeToJson();
        final String expected = Util.doubleQuote("{'schema-version':'0.2','mandatoryRules':{},'immutableRules':{},'contentRules':{},'updateRules':{}}");
        assertEquals(expected, jsonResult);
    }

    @Test
    public void serializeTwoEmptyRules() {
        ValidationRules<ClassOne> cond1 = new ValidationRules<>(ClassOne.class);
        ValidationRules<ClassTwo> cond2 = new ValidationRules<>(ClassTwo.class);
        final String jsonResult = ValidationRules.serializeToJson(cond1, cond2);
        final String expected = Util.doubleQuote("{'schema-version':'0.2','mandatoryRules':{},'immutableRules':{},'contentRules':{},'updateRules':{}}");
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
        final String expected = Util.doubleQuote("{'schema-version':'0.2'," +
                "'mandatoryRules':{'classone':{'stringArrayProp':[]}}," +
                "'immutableRules':{'classone':{'stringProp':[]},'classtwo':{'stringProp':[]}}," +
                "'contentRules':{'classtwo':{'stringProp':[{'constraint':{'type':'EQUALS_ANY','values':['Foo']}}]}}," +
                "'updateRules':{}}");
        assertEquals(expected, jsonResult);

    }

    @Test
    public void manyManyMore() {
        assertTrue(true);
    }


    class ClassOne {
        private String stringProp;
        private String[] stringArrayProp;
        private List<String> stringListProp;
        private List<? extends Number> extNumber;
        private List<? super Integer> supInteger = new ArrayList<>();
        public String getStringProp() {
            return stringProp;
        }
        public String[] getStringArrayProp() {
            return stringArrayProp;
        }
        public List<String> getStringListProp() {
            return stringListProp;
        }
        public List<? extends Number> getExtNumber() {
            return extNumber;
        }
        public List<? super Integer> getSupInteger() {
            return supInteger;
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

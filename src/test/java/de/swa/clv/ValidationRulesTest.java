package de.swa.clv;

import de.swa.clv.constraints.*;
import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static de.swa.clv.ValidationRules.SCHEMA_VERSION;
import static org.junit.jupiter.api.Assertions.*;

public class ValidationRulesTest {

    private static final String SCHEMA_VERSION_JSON = Util.doubleQuote("{'schemaVersion':'" + SCHEMA_VERSION + "',");

    @Test
    public void exceptionIfUnknownProperty() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> rules.mandatory("unknownProperty"));

        assertEquals("No no-arg getter found for property 'unknownProperty' in " +
                "de.swa.clv.ValidationRulesTest$ClassOne", ex.getMessage());
    }

    @Test
    public void exceptionIfPropertyHasWildcardTypeWithLowerBounds() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> rules.mandatory("supInteger[*]"));

        assertEquals("Index definitions for generics with lower bounds wildcard type is not implemented (and " +
                        "quite useless(?)): supInteger[*]", ex.getMessage());
    }

    @Test
    public void exceptionIfPropertyWithIndexDefinitionHasWrongType() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> rules.mandatory("stringProp[0]"));

        assertEquals("Index definitions are only allowed for properties of type List or arrays: stringProp[0]",
                ex.getMessage());
    }

    @Test
    public void mandatoryIndexedPropertiesEverywhere() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        try {
            rules.mandatory("stringArrayProp[1-3]", Condition.of("stringArrayProp[4]",
                    Equals.anyRef("stringArrayProp[5,6]")));
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
    }

    @Test
    public void content_sum_integer_range_max() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        try {
            rules.content("integerList[*]#sum", Range.max(10));
        } catch (Exception e) {
            e.printStackTrace();
            fail("" + e.getMessage());
        }
    }

    @Test
    public void content_sum_integer_equals_string_fails() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> rules.content("integerList[*]#sum", Equals.any("wrong type")));

        assertEquals("Constraint EqualsAny does not support type of property integerList[*]#sum " +
                        "(class java.lang.Integer)",
                ex.getMessage());
    }

    @Test
    public void content_distinct_integer_equals_true() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        try {
            rules.content("integerList[*]#distinct", Equals.any(true));
        } catch (Exception e) {
            e.printStackTrace();
            fail("" + e.getMessage());
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
    public void mandatoryObjectListProperty() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        try {
            rules.mandatory("uuidList[*]");
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    @Test
    public void content_localDate_quarterAny() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        try {
            rules.content("localDateProp", Quarter.any(2, 4));
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }


    @Test
    public void doNotSerializeForAllRuleTypesForAllButOneRule() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
            rules.mandatory("stringProp").doNotSerialize();
            rules.immutable("stringProp", Permissions.all("A", "B")).doNotSerialize();
            rules.content("stringProp", Equals.notNull()).doNotSerialize();
            rules.content("stringProp", Size.min(123));
            rules.update("stringProp", Equals.notNull(), Condition.of("stringProp", Equals.notNull())).doNotSerialize();
        final String jsonResult = rules.serializeToJson();
        final String expected = SCHEMA_VERSION_JSON + """
                "mandatoryRules":{},
                "immutableRules":{},
                "contentRules":{"classone":{"stringProp":[{"constraint":{"type":"SIZE","min":123}}]}},
                "updateRules":{}}
                """.replace("\n", "");
        assertEquals(expected, jsonResult);
    }


    @Test
    public void serializeEmptyRulesInstance() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        final String jsonResult = rules.serializeToJson();
        final String expected = SCHEMA_VERSION_JSON + """
                "mandatoryRules":{},
                "immutableRules":{},
                "contentRules":{},
                "updateRules":{}}
                """.replace("\n", "");
        assertEquals(expected, jsonResult);
    }

    @Test
    public void serializeTwoEmptyRules() {
        ValidationRules<ClassOne> cond1 = new ValidationRules<>(ClassOne.class);
        ValidationRules<ClassTwo> cond2 = new ValidationRules<>(ClassTwo.class);
        final String jsonResult = ValidationRules.serializeToJson(cond1, cond2);
        final String expected = SCHEMA_VERSION_JSON + """
                "mandatoryRules":{},
                "immutableRules":{},
                "contentRules":{},
                "updateRules":{}}
                """.replace("\n", "");
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
        final String expected = SCHEMA_VERSION_JSON + """
                "mandatoryRules":{"classone":{"stringArrayProp":[]}},
                "immutableRules":{"classone":{"stringProp":[]},"classtwo":{"stringProp":[]}},
                "contentRules":{"classtwo":{"stringProp":[{"constraint":{"type":"EQUALS_ANY","values":["Foo"]}}]}},
                "updateRules":{}}
                """.replace("\n", "");
        assertEquals(expected, jsonResult);

    }

    @Test
    public void serializePropertyWithMultipleRulesOneEmpty() {
        ValidationRules<ClassOne> cond1 = new ValidationRules<>(ClassOne.class);
        cond1.immutable("stringProp", Permissions.any("FOO"));
        cond1.immutable("stringProp", Permissions.any("BAR"));
        cond1.immutable("stringProp");
        String jsonResult = ValidationRules.serializeToJson(cond1);
        final String expected = SCHEMA_VERSION_JSON + """
                "mandatoryRules":{},
                "immutableRules":{"classone":{"stringProp":[{"permissions":{"type":"ANY","values":["FOO"]}},
                {"permissions":{"type":"ANY","values":["BAR"]}},{}]}},
                "contentRules":{},
                "updateRules":{}}
                """.replace("\n", "");
        assertEquals(expected, jsonResult);
    }

    @Test
    public void manyManyMore() {
        assertTrue(true);
    }

    static class ClassOne {
        private String stringProp;
        private String[] stringArrayProp;
        private List<String> stringListProp;
        private List<? extends Number> extNumber;
        private final List<? super Integer> supInteger = new ArrayList<>();
        private final List<Integer> integerList = new ArrayList<>();
        private final List<UUID> uuidList = new ArrayList<>();
        private final LocalDate localDateProp = LocalDate.of(2022, 12, 31);
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
        public List<UUID> getUuidList() {
            return uuidList;
        }
        public List<Integer> getIntegerList() {return integerList;}
        public LocalDate getLocalDateProp() {
            return localDateProp;
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

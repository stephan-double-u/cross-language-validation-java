package de.swa.clv;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.networknt.schema.JsonSchema;
import com.networknt.schema.JsonSchemaFactory;
import com.networknt.schema.SpecVersion;
import com.networknt.schema.ValidationMessage;
import de.swa.clv.constraints.*;
import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import org.slf4j.LoggerFactory;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;

import static de.swa.clv.ValidationRules.SCHEMA_VERSION;
import static org.junit.jupiter.api.Assertions.*;

public class ValidationRulesTest {

    private static final String SCHEMA_VERSION_JSON = Util.doubleQuote("{'schemaVersion':'" + SCHEMA_VERSION + "',");

    private final JsonSchemaFactory schemaFactory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V201909);
    private final InputStream schemaStream = inputStreamFromClasspath("schema/cross-language-validation-schema.json");
    private final JsonSchema schema = schemaFactory.getSchema(schemaStream);
    private final ObjectMapper objectMapper = new ObjectMapper();

    ValidationRulesTest() throws IOException {
    }

    private static InputStream inputStreamFromClasspath(String path) {
        return Thread.currentThread().getContextClassLoader().getResourceAsStream(path);
    }

    @Test
    void mandatory_valueChangedNotAllowed() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> rules.mandatory("stringProp",
                        Condition.of("stringProp", Value.changed())));

        assertEquals("The constraint ValueChanged is not allowed for rules of type MANDATORY",
                ex.getMessage());
    }

    @Test
    void mandatory_valueUnchangedNotAllowed() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> rules.mandatory("stringProp",
                        Condition.of("stringProp", Value.unchanged())));

        assertEquals("The constraint ValueUnchanged is not allowed for rules of type MANDATORY",
                ex.getMessage());
    }

    @Test
    void content_valueChangedNotAllowed() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> rules.content("stringProp", Equals.notNull(),
                        Condition.of("stringProp", Value.changed())));

        assertEquals("The constraint ValueChanged is not allowed for rules of type CONTENT",
                ex.getMessage());
    }

    @Test
    void content_valueUnchangedNotAllowed() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> rules.content("stringProp", Equals.notNull(),
                        Condition.of("stringProp", Value.unchanged())));

        assertEquals("The constraint ValueUnchanged is not allowed for rules of type CONTENT",
                ex.getMessage());
    }

    @Test
    void exceptionIfUnknownProperty() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> rules.mandatory("unknownProperty"));

        assertEquals("No no-arg getter found for property 'unknownProperty' in " +
                "de.swa.clv.ValidationRulesTest$ClassOne", ex.getMessage());
    }

    @Test
    void exceptionIfPropertyHasWildcardTypeWithLowerBounds() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> rules.mandatory("supInteger[*]"));

        assertEquals("Index definitions for generics with lower bounds wildcard type is not implemented (and " +
                        "quite useless(?)): supInteger[*]", ex.getMessage());
    }

    @Test
    void exceptionIfPropertyWithIndexDefinitionHasWrongType() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> rules.mandatory("stringProp[0]"));

        assertEquals("Index definitions are only allowed for properties of type List or arrays: stringProp[0]",
                ex.getMessage());
    }

    @Test
    void mandatoryIndexedPropertiesEverywhere() {
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
    void content_sum_integer_range_max() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        try {
            rules.content("integerList[*]#sum", Range.max(10));
        } catch (Exception e) {
            e.printStackTrace();
            fail("" + e.getMessage());
        }
    }

    @Test
    void content_sum_integer_equals_string_fails() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> rules.content("integerList[*]#sum", Equals.any("wrong type")));

        assertEquals("Constraint EqualsAny does not support type of property integerList[*]#sum " +
                        "(class java.lang.Integer)",
                ex.getMessage());
    }

    @Test
    void content_distinct_integer_equals_true() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        try {
            rules.content("integerList[*]#distinct", Equals.any(true));
        } catch (Exception e) {
            e.printStackTrace();
            fail("" + e.getMessage());
        }
    }

    @Test
    void mandatoryIndexedPropertyWildcardUpperBounds() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        try {
            rules.mandatory("extNumber[*]");
        } catch (Exception e) {
            fail();
        }
    }

    @Test
    void mandatoryObjectListProperty() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        try {
            rules.mandatory("uuidList[*]");
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    @Test
    void content_localDate_quarterAny() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        try {
            rules.content("localDateProp", Quarter.any(2, 4));
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    @Test
    void updateRuleWithRefConstraintForUpdateWithoutConditionConstraintShouldLogInfo() {
        Logger log = (Logger) LoggerFactory.getLogger(ValidationRules.class);
        ListAppender<ILoggingEvent> listAppender = new ListAppender<>();
        listAppender.start();
        log.addAppender(listAppender);
        String expectedInfoLog = "The update rule for the property 'localDateProp' of type 'ClassOne' has a " +
                "property constraint that references the 'update entity' but has no condition constraint. " +
                "Therefore, it should better be written as a content rule.";
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);

        rules.update("localDateProp", Equals.anyRef("localDateProp"));

        List<ILoggingEvent> logsList = listAppender.list;
        assertEquals(Level.INFO, logsList.get(0).getLevel());
        assertEquals(expectedInfoLog, logsList.get(0).getMessage());
    }


    @Test
    void serializeEmptyRulesInstance() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        String expected = SCHEMA_VERSION_JSON + """
                "mandatoryRules":{},
                "immutableRules":{},
                "contentRules":{},
                "updateRules":{}}
                """.replace("\n", "");

        String jsonResult = rules.serializeToJson();

        assertEquals(expected, jsonResult);
        assertJsonIsValid(jsonResult);
    }

    @Test
    void serializeTwoEmptyRules() {
        ValidationRules<ClassOne> cond1 = new ValidationRules<>(ClassOne.class);
        ValidationRules<ClassTwo> cond2 = new ValidationRules<>(ClassTwo.class);
        String expected = SCHEMA_VERSION_JSON + """
                "mandatoryRules":{},
                "immutableRules":{},
                "contentRules":{},
                "updateRules":{}}
                """.replace("\n", "");

        String jsonResult = ValidationRules.serializeToJson(cond1, cond2);

        assertEquals(expected, jsonResult);
        assertJsonIsValid(jsonResult);
    }

    @Test
    void serializeTwoRules() {
        ValidationRules<ClassOne> cond1 = new ValidationRules<>(ClassOne.class);
        cond1.mandatory("stringArrayProp");
        cond1.immutable("stringProp");
        ValidationRules<ClassTwo> cond2 = new ValidationRules<>(ClassTwo.class);
        cond2.immutable("stringProp");
        cond2.content("stringProp", Equals.any("Foo"));
        String expected = SCHEMA_VERSION_JSON + """
                "mandatoryRules":{"classone":{"stringArrayProp":[]}},
                "immutableRules":{"classone":{"stringProp":[]},"classtwo":{"stringProp":[]}},
                "contentRules":{"classtwo":{"stringProp":[{"constraint":{"type":"EQUALS_ANY","values":["Foo"]}}]}},
                "updateRules":{}}
                """.replace("\n", "");

        String jsonResult = ValidationRules.serializeToJson(cond1, cond2);

        assertEquals(expected, jsonResult);
        assertJsonIsValid(jsonResult);
    }

    @Test
    void serializePropertyWithMultipleRulesOneEmpty() {
        ValidationRules<ClassOne> cond1 = new ValidationRules<>(ClassOne.class);
        cond1.immutable("stringProp", Permissions.any("FOO"));
        cond1.immutable("stringProp", Permissions.any("BAR"));
        cond1.immutable("stringProp");
        String expected = SCHEMA_VERSION_JSON + """
                "mandatoryRules":{},
                "immutableRules":{"classone":{"stringProp":[
                {"permissions":{"type":"ANY","values":["FOO"]}},
                {"permissions":{"type":"ANY","values":["BAR"]}},{}]}},
                "contentRules":{},
                "updateRules":{}}
                """.replace("\n", "");

        String jsonResult = ValidationRules.serializeToJson(cond1);

        assertEquals(expected, jsonResult);
        assertJsonIsValid(jsonResult);
    }

    @Test
    void doNotSerializeForAllRuleTypesForAllButOneRule() {
        ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
        rules.mandatory("stringProp").doNotSerialize();
        rules.immutable("stringProp", Permissions.all("A", "B")).doNotSerialize();
        rules.content("stringProp", Equals.notNull()).doNotSerialize();
        rules.content("stringProp", Size.min(123));
        rules.update("stringProp", Equals.notNull(), Condition.of("stringProp", Equals.notNull())).doNotSerialize();

        String expected = SCHEMA_VERSION_JSON + """
                "mandatoryRules":{},
                "immutableRules":{},
                "contentRules":{"classone":{"stringProp":[{"constraint":{"type":"SIZE","min":123}}]}},
                "updateRules":{}}
                """.replace("\n", "");

        String jsonResult = rules.serializeToJson();

        assertEquals(expected, jsonResult);
        assertJsonIsValid(jsonResult);
    }

    private void assertJsonIsValid(String jsonResult) {
        try {
            JsonNode json = objectMapper.readTree(jsonResult);

            Set<ValidationMessage> validationMsgs = schema.validate(json);

            Supplier<String> stringSupplier = () -> validationMsgs.stream()
                    .map(ValidationMessage::getMessage)
                    .collect(Collectors.joining("\n"));

            assertTrue(validationMsgs.isEmpty(), stringSupplier);
        } catch (JsonProcessingException e) {
            fail(e);
        }
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

    static class ClassTwo {
        private String stringProp;
        public String getStringProp() {
            return stringProp;
        }
    }

    enum Perms {
        PERM1, PERM2
    }
}

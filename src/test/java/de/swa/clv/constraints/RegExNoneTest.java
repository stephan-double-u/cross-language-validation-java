package de.swa.clv.constraints;

import org.junit.jupiter.api.Test;

import static de.swa.clv.constraints.Constraint.NULL_VALUE_ERR_MESSAGE;
import static de.swa.clv.test.Util.doubleQuote;
import static java.lang.Boolean.TRUE;
import static org.junit.jupiter.api.Assertions.*;

class RegExNoneTest {

    private static RegExNone regEx = RegEx.none("bar", "(?i)[o]{2}", "^[0-7]+$", "\\p{N}");
    private static RegExNone regExNorNull = RegEx.noneNorNull("bar");

    @Test
    void exceptionIfRegExIsNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> RegEx.none("[a-c]+.", null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void unsupportedType() {
        assertThrows(IllegalArgumentException.class,
                () -> regEx.validate(TRUE, null));
    }

    @Test
    void stringMatchFirstRegEx() {
        assertFalse(regEx.validate("foobarzoo", null));
    }

    @Test
    void stringMatchSecondRegEx() {
        assertFalse(regEx.validate("fOOzOO", null));
    }

    @Test
    void enumMatchSecondRegEx() {
        assertFalse(regEx.validate(FooBarZoo.ZOO, null));
    }

    @Test()
    void numberMatchThirdRegEx() {
        assertFalse(regEx.validate(1234567, null));
    }

    @Test
    void noRegExMatch() {
        assertTrue(regEx.validate("something", null));
    }

    @Test
    void nullValueFalse() {
        assertFalse(regExNorNull.validate(null, null));
    }

    @Test
    void nullValueTrue() {
        assertFalse(regEx.validate(null, null));
    }

    @Test
    void serializeToJson() {
        assertEquals("""
                "type":"REGEX_NONE","values":["bar","(?i)[o]{2}","^[0-7]+$","\\\\p{N}"],"nullEqualsTo":true""",
                regEx.serializeToJson());
    }

    @Test
    void serializeToJsonNorNull() {
        assertEquals("""
                "type":"REGEX_NONE","values":["bar"]""", regExNorNull.serializeToJson());
    }

    enum FooBarZoo {
    FOO, BAR, ZOO
    }
}

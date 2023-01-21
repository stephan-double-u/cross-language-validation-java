package de.swa.clv.constraints;

import org.junit.jupiter.api.Test;

import static de.swa.clv.constraints.Constraint.NULL_VALUE_ERR_MESSAGE;
import static de.swa.clv.test.Util.doubleQuote;
import static java.lang.Boolean.TRUE;
import static org.junit.jupiter.api.Assertions.*;

class RegExAnyTest {

    private static final RegExAny regEx = RegEx.any("bar", "(?i)[o]{2}", "^[0-7]+$", "\\p{N}");
    private static final RegExAny regExOrNull = RegEx.anyOrNull("[0-9]+");

    @Test
    void exceptionIfRegExIsNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> RegEx.any("[a-c]+.", null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void unsupportedType() {
        assertThrows(IllegalArgumentException.class,
                () -> regEx.validate(TRUE, null));
    }

    @Test
    void stringMatchFirstRegEx() {
        assertTrue(regEx.validate("foobarzoo", null));
    }

    @Test
    void stringMatchSecondRegEx() {
        assertTrue(regEx.validate("fOOzOO", null));
    }

    @Test
    void enumMatchSecondRegEx() {
        assertTrue(regEx.validate(FooBarZoo.ZOO, null));
    }

    @Test()
    void numberMatchThirdRegEx() {
        assertTrue(regEx.validate(1234567, null));
    }

    @Test
    void noRegExMatch() {
        assertFalse(regEx.validate("something", null));
    }

    @Test
    void nullValueFalse() {
        assertFalse(regEx.validate(null, null));
    }

    @Test
    void nullValueTrue() {
        assertFalse(regExOrNull.validate(null, null));
    }

    @Test
    void serializeToJson() {
        assertEquals("""
                "type":"REGEX_ANY","values":["bar","(?i)[o]{2}","^[0-7]+$","\\\\p{N}"]""", regEx.serializeToJson());
    }

    @Test
    void serializeToJsonOrNull() {
        assertEquals("""
                "type":"REGEX_ANY","values":["[0-9]+"],"nullEqualsTo":true""", regExOrNull.serializeToJson());
    }

    enum FooBarZoo {
    FOO, BAR, ZOO
    }
}

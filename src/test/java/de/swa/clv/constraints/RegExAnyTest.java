package de.swa.clv.constraints;

import org.junit.jupiter.api.Test;

import static de.swa.clv.constraints.Constraint.NULL_VALUE_ERR_MESSAGE;
import static de.swa.clv.test.Util.doubleQuote;
import static java.lang.Boolean.TRUE;
import static org.junit.jupiter.api.Assertions.*;

public class RegExAnyTest {

    private static RegExAny regEx = RegEx.any("bar", "(?i)[o]{2}", "^[0-7]+$", "\\p{N}");

    @Test
    public void exceptionIfRegExIsNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> RegEx.any("[a-c]+.", null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    public void unsupportedType() {
        assertThrows(IllegalArgumentException.class,
                () -> regEx.validate(TRUE, null));
    }

    @Test
    public void stringMatchFirstRegEx() {
        assertTrue(regEx.validate("foobarzoo", null));
    }

    @Test
    public void stringMatchSecondRegEx() {
        assertTrue(regEx.validate("fOOzOO", null));
    }

    @Test
    public void enumMatchSecondRegEx() {
        assertTrue(regEx.validate(FooBarZoo.ZOO, null));
    }

    @Test()
    public void numberMatchThirdRegEx() {
        assertTrue(regEx.validate(1234567, null));
    }

    @Test
    public void noRegExMatch() {
        assertFalse(regEx.validate("something", null));
    }

    @Test
    public void serializeToJson() {
        assertEquals(doubleQuote("'type':'REGEX_ANY','values':['bar','(?i)[o]{2}','^[0-7]+$','\\\\p{N}']"),
                regEx.serializeToJson());
    }

    enum FooBarZoo {
    FOO, BAR, ZOO
    }
}

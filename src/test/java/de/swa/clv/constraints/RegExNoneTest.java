package de.swa.clv.constraints;

import org.junit.Test;

import static de.swa.clv.constraints.ConstraintRoot.NULL_VALUE_ERR_MESSAGE;
import static de.swa.clv.test.Util.doubleQuote;
import static java.lang.Boolean.TRUE;
import static org.junit.Assert.*;

public class RegExNoneTest {

    private static RegExNone regEx = RegEx.none("bar", "(?i)[o]{2}", "^[0-7]+$", "\\p{N}");

    @Test
    public void exceptionIfRegExIsNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> RegEx.none("[a-c]+.", null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test(expected = IllegalArgumentException.class)
    public void unsupportedType() {
        regEx.validate(TRUE, null);
    }

    @Test
    public void stringMatchFirstRegEx() {
        assertFalse(regEx.validate("foobarzoo", null));
    }

    @Test
    public void stringMatchSecondRegEx() {
        assertFalse(regEx.validate("fOOzOO", null));
    }

    @Test
    public void enumMatchSecondRegEx() {
        assertFalse(regEx.validate(FooBarZoo.ZOO, null));
    }

    @Test()
    public void numberMatchThirdRegEx() {
        assertFalse(regEx.validate(1234567, null));
    }

    @Test
    public void noRegExMatch() {
        assertTrue(regEx.validate("something", null));
    }

    @Test
    public void serializeToJson() {
        assertEquals(doubleQuote("'type':'REGEX_NONE','values':['bar','(?i)[o]{2}','^[0-7]+$','\\\\p{N}']"),
                regEx.serializeToJson());
    }

    enum FooBarZoo {
    FOO, BAR, ZOO
    }
}

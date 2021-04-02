package de.swa.clv.constraints;

import org.hamcrest.core.StringContains;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static de.swa.clv.test.Util.doubleQuote;
import static java.lang.Boolean.TRUE;
import static org.junit.Assert.*;

public class RegExTest {

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    private static RegEx regEx = RegEx.any("bar", "[o]{2}", "^[0-7]+$");

    @Test
    public void exceptionIfRegExIsNull() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Null values are not allowed"));
        RegEx.any("[a-c]+.", null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void unsupportedType() {
        regEx.validate(TRUE, null);
    }

    @Test
    public void stringMatchFirstRegEx() {
        assertTrue(regEx.validate("foobarzoo", null));
    }

    @Test
    public void stringMatchSecondRegEx() {
        assertTrue(regEx.validate("foozoo", null));
    }

    @Test()
    public void numberMatchThirsRegEx() {
        assertTrue(regEx.validate(1234567, null));
    }

    @Test
    public void noRegExMatch() {
        assertFalse(regEx.validate("something", null));
    }

    @Test
    public void serializeToJson() {
        assertEquals(doubleQuote("'type':'REGEX_ANY','values':['bar','[o]{2}','^[0-7]+$']"), regEx.serializeToJson());
    }
}

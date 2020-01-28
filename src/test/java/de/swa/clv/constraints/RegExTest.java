package de.swa.clv.constraints;

import org.hamcrest.core.StringContains;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static de.swa.clv.test.Util.doubleQuote;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class RegExTest {

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    private static RegEx regEx = RegEx.any("bar", "[o]{2}");

    @Test
    public void exceptionIfRegExIsNull() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Null values are not allowed"));
        RegEx.any("[a-c]+.", null);
    }

    @Test
    public void firstRegExMatch() {
        assertTrue(regEx.validate("foobarzoo", null));
    }

    @Test
    public void secondRegExMatch() {
        assertTrue(regEx.validate("foozoo", null));
    }

    @Test
    public void noRegExMatch() {
        assertFalse(regEx.validate("something", null));
    }

    @Test
    public void serializeToJson() {
        assertEquals(doubleQuote("'type':'REGEX_ANY','values':['bar','[o]{2}']"), regEx.serializeToJson());
    }
}

package de.swa.easyvalidation.constraints;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class RegExpTest {

    // TODO more and better tests ...

    private static RegExp regExp = RegExp.any("[a-c]+.");

    @Test
    public void validate() {
        assertTrue(regExp.validate("abbabcx", null));
    }

    @Test
    public void serializeToJson() {
        assertEquals("{\"type\":\"REGEXP\",\"any\":[\"[a-c]+.\"]}", regExp.serializeToJson());

    }
}

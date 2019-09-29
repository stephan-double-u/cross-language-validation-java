package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.test.Util;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class EqualsNotNullTest {

    @Test
    public void serializeToJson() {
        EqualsNotNull notNull = Equals.notNull();
        assertEquals(Util.doubleQuote("'type':'EQUALS_NOT_NULL'"), notNull.serializeToJson());
    }

}
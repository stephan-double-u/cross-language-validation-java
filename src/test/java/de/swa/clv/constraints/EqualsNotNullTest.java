package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class EqualsNotNullTest {

    @Test
    public void serializeToJson() {
        EqualsNotNull notNull = Equals.notNull();
        assertEquals(Util.doubleQuote("'type':'EQUALS_NOT_NULL'"), notNull.serializeToJson());
    }

}
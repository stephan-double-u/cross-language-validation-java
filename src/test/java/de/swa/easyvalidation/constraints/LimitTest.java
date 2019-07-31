package de.swa.easyvalidation.constraints;

import org.junit.Test;

import static org.junit.Assert.*;

public class LimitTest {

    // TODO more and better tests ...

    @Test
    public void tests() {
        Limit limit = Limit.minMax(123, 1234567890);
        assertEquals("\"type\":\"LIMIT\",\"min\":123,\"max\":1234567890", limit.serializeToJson());

        assertTrue(limit.validate(12345, null));
        assertFalse(limit.validate(0, null));
        assertFalse(limit.validate(Long.MAX_VALUE, null));
    }
}
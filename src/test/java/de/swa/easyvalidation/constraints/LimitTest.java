package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.test.Util;
import org.junit.Test;

import java.math.BigInteger;

import static org.junit.Assert.*;

public class LimitTest {

    private static Limit limit = Limit.minMax(123, 1234567890);

    @Test
    public void validateBigIntegerTrue() {
        assertTrue(limit.validate(BigInteger.valueOf(123L), null));
    }

    @Test
    public void validateBigIntegerFalse() {
        assertFalse(limit.validate(BigInteger.valueOf(Long.MAX_VALUE), null));
    }

    @Test
    public void validateLongTrue() {
        assertTrue(limit.validate(1234567890L, null));
    }

    @Test
    public void validateLongFalse() {
        assertFalse(limit.validate(Long.MAX_VALUE, null));
    }

    @Test
    public void validateDoubleTrue() {
        assertTrue(limit.validate(123.1d, null));
    }

    @Test
    public void validateDoubleFalse() {
        assertFalse(limit.validate(122.9d, null));
    }

    @Test
    public void validateByteTrue() {
        assertTrue(limit.validate((byte) 123, null));
    }

    @Test
    public void validateByteFalse() {
        assertFalse(limit.validate((byte) 122, null));
    }

    @Test
    public void serializeToJson() {
        Limit limit = Limit.minMax(123, 1234567890);
        assertEquals(Util.doubleQuote("'type':'LIMIT','min':123,'max':1234567890"), limit.serializeToJson());
    }
}
package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.test.Util;
import org.junit.Test;

import java.math.BigInteger;

import static org.junit.Assert.*;

public class RangeAnyTest {

    @Test
    public void validateBigIntegerTrue() {
        RangeAny range = Range.min(BigInteger.TEN);
        assertTrue(range.validate(BigInteger.valueOf(123L), null));
    }

    @Test
    public void validateBigIntegerFalse() {
        RangeAny range = Range.max(BigInteger.TEN);
        assertFalse(range.validate(BigInteger.valueOf(Long.MAX_VALUE), null));
    }

    @Test
    public void validateLongTrue() {
        RangeAny range = Range.minMax(123L, 1234567890L);
        assertTrue(range.validate(1234567890L, null));
    }

    @Test
    public void validateLongFalse() {
        RangeAny range = Range.minMax(123L, 1234567890L);
        assertFalse(range.validate(Long.MAX_VALUE, null));
    }

    @Test
    public void validateDoubleTrue() {
        RangeAny range = Range.min(0.1d);
        assertTrue(range.validate(123.1d, null));
    }

    @Test
    public void validateDoubleFalse() {
        RangeAny range = Range.max(0.1d);
        assertFalse(range.validate(122.9d, null));
    }

    @Test
    public void validateByteTrue() {
        RangeAny range = Range.min((byte) 1);
        assertTrue(range.validate((byte) 123, null));
    }

    @Test
    public void validateByteFalse() {
        RangeAny range = Range.max((byte) 1);
        assertFalse(range.validate((byte) 122, null));
    }

    @Test
    public void serializeToJson() {
        RangeAny range = Range.minMax(123, 1234567890);
        assertEquals(Util.doubleQuote("'type':'RANGE_ANY','minValues':[123],'maxValues':[1234567890]"), range.serializeToJson());
    }
}
package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;

import static org.junit.Assert

.*;

public class RangeTest {

    @Test
    public void minIsGreaterThanMax_number() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () ->
                Range.minMax((byte) 1, (byte) 0)
        );
        assertEquals("Range min/max values must be min <= max", ex.getMessage());
    }

    @Test
    public void minIsGreaterThanMax_localDate() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () ->
                Range.minMax(LocalDate.MAX, LocalDate.MIN)
        );
        assertEquals("Range min/max values must be min <= max", ex.getMessage());
    }

    @Test
    public void isSupportedType_Byte() {
        Range range = Range.min((byte) 1);
        assertTrue(range.isSupportedType(Byte.class));
        assertTrue(range.isSupportedType(byte.class));
    }

    @Test
    public void isSupportedType_Short() {
        Range range = Range.min((short) 1);
        assertTrue(range.isSupportedType(Short.class));
        assertTrue(range.isSupportedType(short.class));
    }

    @Test
    public void isSupportedType_Integer() {
        Range range = Range.minMax(1, 2);
        assertTrue(range.isSupportedType(Integer.class));
        assertTrue(range.isSupportedType(int.class));
    }

    @Test
    public void isSupportedType_Long() {
        Range range = Range.minMax(1L, 2L);
        assertTrue(range.isSupportedType(Long.class));
        assertTrue(range.isSupportedType(long.class));
    }

    @Test
    public void isSupportedType_Float() {
        Range range = Range.minMax(1f, 2f);
        assertTrue(range.isSupportedType(Float.class));
        assertTrue(range.isSupportedType(float.class));
    }

    @Test
    public void isSupportedType_Double() {
        Range range = Range.minMax(1d, 2d);
        assertTrue(range.isSupportedType(Double.class));
        assertTrue(range.isSupportedType(double.class));
    }

    @Test
    public void isSupportedType_BigInteger() {
        Range range = Range.minMax(BigInteger.ONE, BigInteger.ONE);
        assertTrue(range.isSupportedType(BigInteger.class));
    }

    @Test
    public void isSupportedType_BigDecimal() {
        Range range = Range.minMax(BigDecimal.ONE, BigDecimal.TEN);
        assertTrue(range.isSupportedType(BigDecimal.class));
    }

    @Test
    public void isSupportedType_LocalDate() {
        Range range = Range.minMax(LocalDate.now(), LocalDate.now());
        assertTrue(range.isSupportedType(LocalDate.class));
    }

    @Test
    public void isSupportedType_LocalDateTime() {
        Range range = Range.minMax(LocalDateTime.now(), LocalDateTime.now());
        assertTrue(range.isSupportedType(LocalDateTime.class));
    }

    @Test
    public void isSupportedType_ByteVsBigDecimal() {
        Range range = Range.min((byte) 1);
        assertTrue(range.isSupportedType(BigDecimal.class));
    }

    @Test
    public void isSupportedType_BigIntegerVsFloat() {
        Range range = Range.min(BigInteger.TEN);
        assertTrue(range.isSupportedType(Float.class));
    }


    @Test
    public void validateBigIntegerTrue() {
        Range range = Range.min(BigInteger.TEN);
        assertTrue(range.validate(BigInteger.valueOf(123L), null));
    }

    @Test
    public void validateBigIntegerFalse() {
        Range range = Range.max(BigInteger.TEN);
        assertFalse(range.validate(BigInteger.valueOf(Long.MAX_VALUE), null));
    }

    @Test
    public void validateLongTrue() {
        Range range = Range.minMax(123L, 1234567890L);
        assertTrue(range.validate(1234567890L, null));
    }

    @Test
    public void validateLongFalse() {
        Range range = Range.minMax(123L, 1234567890L);
        assertFalse(range.validate(Long.MAX_VALUE, null));
    }

    @Test
    public void validateDoubleTrue() {
        Range range = Range.min(0.1d);
        assertTrue(range.validate(123.1d, null));
    }

    @Test
    public void validateDoubleFalse() {
        Range range = Range.max(0.1d);
        assertFalse(range.validate(122.9d, null));
    }

    @Test
    public void validateByteTrue() {
        Range range = Range.min((byte) 1);
        assertTrue(range.validate((byte) 123, null));
    }

    @Test
    public void validateByteFalse() {
        Range range = Range.max((byte) 1);
        assertFalse(range.validate((byte) 122, null));
    }

    @Test
    public void validateBigIntegerVsByteTrue() {
        Range range = Range.max(BigInteger.TEN);
        assertTrue(range.validate((byte) 10, null));
    }

    @Test
    public void validateBigIntegerVsByteFalse() {
        Range range = Range.max(BigInteger.TEN);
        assertFalse(range.validate((byte) 11, null));
    }

    @Test
    public void validateFloatVsBigDecimalTrue() {
        Range range = Range.minMax(1.23f, 4.56f);
        assertTrue(range.validate(BigDecimal.valueOf(123, 2), null));
    }

    @Test
    public void validateFloatVsBigDecimaFalse() {
        Range range = Range.minMax(1.23f, 4.56f);
        assertFalse(range.validate(BigDecimal.valueOf(457, 2), null));
    }

    @Test
    public void validateLocalDateTrue() {
        Range range = Range.minMax(LocalDate.of(2000, Month.JANUARY, 1), LocalDate.MAX);
        assertTrue(range.validate(LocalDate.now(), null));
    }

    @Test
    public void validateLocalDateFalse() {
        Range range = Range.min(LocalDate.now().plusDays(1));
        assertFalse(range.validate(LocalDate.now(), null));
    }

    @Test
    public void validateLocalDateTimeTrue() {
        Range range = Range.minMax(LocalDateTime.MIN, LocalDateTime.MAX);
        assertTrue(range.validate(LocalDateTime.now(), null));
    }

    @Test
    public void validateLocalDateTimeFalse() {
        Range range = Range.min(LocalDateTime.MAX);
        assertFalse(range.validate(LocalDateTime.now(), null));
    }


    @Test
    public void serializeToJsonNumber() {
        Range range = Range.minMax(123, 1234567890);
        assertEquals(Util.doubleQuote("'type':'RANGE','min':123,'max':1234567890"), range.serializeToJson());
    }

    @Test
    public void serializeToJsonLocalDate() {
        Range range = Range.max(LocalDate.of(2000, Month.JANUARY, 1));
        assertEquals(Util.doubleQuote("'type':'RANGE','max':'2000-01-01'"), range.serializeToJson());
    }
}

package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;

import static org.junit.jupiter.api.Assertions.*;

class RangeTest {

    @Test
    void minIsGreaterThanMax_number() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () ->
                Range.minMax((byte) 1, (byte) 0)
        );
        assertEquals("Range min/max values must be min <= max", ex.getMessage());
    }

    @Test
    void minIsGreaterThanMax_localDate() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () ->
                Range.minMax(LocalDate.MAX, LocalDate.MIN)
        );
        assertEquals("Range min/max values must be min <= max", ex.getMessage());
    }

    @Test
    void isSupportedType_Byte() {
        Range range = Range.min((byte) 1);
        assertTrue(range.isSupportedType(Byte.class));
        assertTrue(range.isSupportedType(byte.class));
    }

    @Test
    void isSupportedType_Short() {
        Range range = Range.min((short) 1);
        assertTrue(range.isSupportedType(Short.class));
        assertTrue(range.isSupportedType(short.class));
    }

    @Test
    void isSupportedType_Integer() {
        Range range = Range.minMax(1, 2);
        assertTrue(range.isSupportedType(Integer.class));
        assertTrue(range.isSupportedType(int.class));
    }

    @Test
    void isSupportedType_Long() {
        Range range = Range.minMax(1L, 2L);
        assertTrue(range.isSupportedType(Long.class));
        assertTrue(range.isSupportedType(long.class));
    }

    @Test
    void isSupportedType_Float() {
        Range range = Range.minMax(1f, 2f);
        assertTrue(range.isSupportedType(Float.class));
        assertTrue(range.isSupportedType(float.class));
    }

    @Test
    void isSupportedType_Double() {
        Range range = Range.minMax(1d, 2d);
        assertTrue(range.isSupportedType(Double.class));
        assertTrue(range.isSupportedType(double.class));
    }

    @Test
    void isSupportedType_BigInteger() {
        Range range = Range.minMax(BigInteger.ONE, BigInteger.ONE);
        assertTrue(range.isSupportedType(BigInteger.class));
    }

    @Test
    void isSupportedType_BigDecimal() {
        Range range = Range.minMax(BigDecimal.ONE, BigDecimal.TEN);
        assertTrue(range.isSupportedType(BigDecimal.class));
    }

    @Test
    void isSupportedType_LocalDate() {
        Range range = Range.minMax(LocalDate.now(), LocalDate.now());
        assertTrue(range.isSupportedType(LocalDate.class));
    }

    @Test
    void isSupportedType_LocalDateTime() {
        Range range = Range.minMax(LocalDateTime.now(), LocalDateTime.now());
        assertTrue(range.isSupportedType(LocalDateTime.class));
    }

    @Test
    void isSupportedType_ByteVsBigDecimal() {
        Range range = Range.min((byte) 1);
        assertTrue(range.isSupportedType(BigDecimal.class));
    }

    @Test
    void isSupportedType_BigIntegerVsFloat() {
        Range range = Range.min(BigInteger.TEN);
        assertTrue(range.isSupportedType(Float.class));
    }


    @Test
    void validateNullValueToFalseNumberMin() {
        Range range = Range.min(1);
        assertFalse(range.validate(null, null));
    }

    @Test
    void validateNullValueToTrueNumberMin() {
        Range range = Range.minOrNull(1);
        assertTrue(range.validate(null, null));
    }

    @Test
    void validateNullValueToFalseNumberMax() {
        Range range = Range.max(1);
        assertFalse(range.validate(null, null));
    }

    @Test
    void validateNullValueToTrueNumberMax() {
        Range range = Range.maxOrNull(1);
        assertTrue(range.validate(null, null));
    }

    @Test
    void validateNullValueToFalseNumberMinMax() {
        Range range = Range.minMax(1, 2);
        assertFalse(range.validate(null, null));
    }

    @Test
    void validateNullValueToTrueNumberMinMax() {
        Range range = Range.minMaxOrNull(1, 2);
        assertTrue(range.validate(null, null));
    }

    @Test
    void validateBigIntegerTrue() {
        Range range = Range.min(BigInteger.TEN);
        assertTrue(range.validate(BigInteger.valueOf(123L), null));
    }

    @Test
    void validateBigIntegerFalse() {
        Range range = Range.max(BigInteger.TEN);
        assertFalse(range.validate(BigInteger.valueOf(Long.MAX_VALUE), null));
    }

    @Test
    void validateLongTrue() {
        Range range = Range.minMax(123L, 1234567890L);
        assertTrue(range.validate(1234567890L, null));
    }

    @Test
    void validateMinSaveIntegerFalse() {
        Range range = Range.minMax(Range.MIN_SAVE_INTEGER_JAVASCRIPT, 0L);
        assertFalse(range.validate(Range.MIN_SAVE_INTEGER_JAVASCRIPT - 1, null));
    }

    @Test
    void validateMaxSaveIntegerFalse() {
        Range range = Range.minMax(0L, Range.MAX_SAVE_INTEGER_JAVASCRIPT);
        assertFalse(range.validate(Range.MAX_SAVE_INTEGER_JAVASCRIPT + 1, null));
    }

    @Test
    void validateToFalseIfValueIsSmallerThanMin() {
        Range range = Range.minMax(1, 2);
        assertFalse(range.validate(0, null));
    }

    @Test
    void validateToFalseIfValueIsBiggerThanMax() {
        Range range = Range.minMax(1, 2);
        assertFalse(range.validate(3, null));
    }

    @Test
    void validateDoubleTrue() {
        Range range = Range.min(0.1d);
        assertTrue(range.validate(123.1d, null));
    }

    @Test
    void validateDoubleFalse() {
        Range range = Range.max(0.1d);
        assertFalse(range.validate(122.9d, null));
    }

    @Test
    void validateByteTrue() {
        Range range = Range.min((byte) 1);
        assertTrue(range.validate((byte) 123, null));
    }

    @Test
    void validateByteFalse() {
        Range range = Range.max((byte) 1);
        assertFalse(range.validate((byte) 122, null));
    }

    @Test
    void validateBigIntegerVsByteTrue() {
        Range range = Range.max(BigInteger.TEN);
        assertTrue(range.validate((byte) 10, null));
    }

    @Test
    void validateBigIntegerVsByteFalse() {
        Range range = Range.max(BigInteger.TEN);
        assertFalse(range.validate((byte) 11, null));
    }

    @Test
    void validateFloatVsBigDecimalTrue() {
        Range range = Range.minMax(1.23f, 4.56f);
        assertTrue(range.validate(BigDecimal.valueOf(123, 2), null));
    }

    @Test
    void validateFloatVsBigDecimalFalse() {
        Range range = Range.minMax(1.23f, 4.56f);
        assertFalse(range.validate(BigDecimal.valueOf(457, 2), null));
    }

    @Test
    void validateLocalDateTrue() {
        Range range = Range.minMax(LocalDate.of(2000, Month.JANUARY, 1), LocalDate.MAX);
        assertTrue(range.validate(LocalDate.now(), null));
    }

    @Test
    void validateLocalDateFalse() {
        Range range = Range.min(LocalDate.now().plusDays(1));
        assertFalse(range.validate(LocalDate.now(), null));
    }

    @Test
    void validateLocalDateTimeTrue() {
        Range range = Range.minMax(LocalDateTime.MIN, LocalDateTime.MAX);
        assertTrue(range.validate(LocalDateTime.now(), null));
    }

    @Test
    void validateLocalDateTimeFalse() {
        Range range = Range.min(LocalDateTime.MAX);
        assertFalse(range.validate(LocalDateTime.now(), null));
    }


    @Test
    void serializeToJsonMin() {
        Range range = Range.min(LocalDate.of(2000, Month.JANUARY, 1));
        assertEquals("""
                "type":"RANGE","min":"2000-01-01\"""", range.serializeToJson());
    }

    @Test
    void serializeToJsonMinOrNull() {
        Range range = Range.minOrNull(LocalDate.of(2000, Month.JANUARY, 1));
        assertEquals("""
                "type":"RANGE","min":"2000-01-01","nullEqualsTo":true""", range.serializeToJson());
    }

    @Test
    void serializeToJsonMax() {
        Range range = Range.max(LocalDateTime.of(2000, Month.JANUARY, 1, 20, 59));
        assertEquals("""
                "type":"RANGE","max":"2000-01-01T20:59\"""", range.serializeToJson());
    }

    @Test
    void serializeToJsonMaxOrNull() {
        Range range = Range.maxOrNull(LocalDateTime.of(2000, Month.JANUARY, 1, 20, 59));
        assertEquals("""
                "type":"RANGE","max":"2000-01-01T20:59","nullEqualsTo":true""", range.serializeToJson());
    }

    @Test
    void serializeToJsonMinMax() {
        Range range = Range.minMax(123, 1234567890);
        assertEquals("""
                "type":"RANGE","min":123,"max":1234567890""", range.serializeToJson());
    }

    @Test
    void serializeToJsonMinMaxOrNull() {
        Range range = Range.minMaxOrNull(123, 1234567890);
        assertEquals("""
                "type":"RANGE","min":123,"max":1234567890,"nullEqualsTo":true""", range.serializeToJson());
    }

}

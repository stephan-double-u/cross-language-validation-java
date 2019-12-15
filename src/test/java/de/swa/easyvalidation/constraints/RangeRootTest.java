package de.swa.easyvalidation.constraints;

import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.JDBCType;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class RangeRootTest {

    @Test
    public void isSupportedType_Byte() {
        RangeRoot range = Range.minAny((byte) 1, (byte) 2).maxAny((byte) 8, (byte) 9);

        assertTrue(range.isSupportedType(Byte.class));
        assertTrue(range.isSupportedType(byte.class));
    }

    @Test
    public void isSupportedType_Short() {
        RangeRoot range = Range.minAny((short) 1, (short) 2).maxAny((short) 8, (short) 9);

        assertTrue(range.isSupportedType(Short.class));
        assertTrue(range.isSupportedType(short.class));
    }

    @Test
    public void isSupportedType_Integer() {
        RangeRoot range = Range.minAny(1, 2).maxAny(8, 9);

        assertTrue(range.isSupportedType(Integer.class));
        assertTrue(range.isSupportedType(int.class));
    }

    @Test
    public void isSupportedType_Long() {
        RangeRoot range = Range.minAny(1L, 2L).maxAny(8L, 9L);

        assertTrue(range.isSupportedType(Long.class));
        assertTrue(range.isSupportedType(long.class));
    }

    @Test
    public void isSupportedType_Float() {
        RangeRoot range = Range.minAny(1f, 2f).maxAny(8f, 9f);

        assertTrue(range.isSupportedType(Float.class));
        assertTrue(range.isSupportedType(float.class));
    }

    @Test
    public void isSupportedType_Double() {
        RangeRoot range = Range.minAny(1d, 2d).maxAny(8d, 9d);

        assertTrue(range.isSupportedType(Double.class));
        assertTrue(range.isSupportedType(double.class));
    }

    @Test
    public void isSupportedType_BigInteger() {
        RangeRoot range = Range.minAny(BigInteger.ONE, BigInteger.TWO).maxAny(BigInteger.TEN);

        assertTrue(range.isSupportedType(BigInteger.class));
    }

    @Test
    public void isSupportedType_BigDecimal() {
        RangeRoot range = Range.minAny(BigDecimal.ONE).maxAny(BigDecimal.TEN);

        assertTrue(range.isSupportedType(BigDecimal.class));
    }

    @Test
    public void isSupportedType_String() {
        RangeRoot range = Range.min("Foo").maxAny("X", "Y").use(ComparisonType.LEXICOGRAPHICAL_UNICODE);

        assertTrue(range.isSupportedType(String.class));
    }

    @Test
    public void stringNotSupportedForInteger() {
        RangeRoot range = Range.min("Foo");

        assertFalse(range.isSupportedType(Integer.class));
    }

    @Test
    public void integerNotSupportedForString() {
        RangeRoot range = Range.min(1);

        assertFalse(range.isSupportedType(String.class));
    }

    @Test
    public void integerNotSupportedForLong() {
        RangeRoot range = Range.min(1L);

        assertFalse(range.isSupportedType(Integer.class));
    }

}

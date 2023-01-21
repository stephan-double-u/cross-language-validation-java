package de.swa.clv.constraints;

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static de.swa.clv.test.Util.doubleQuote;
import static org.junit.jupiter.api.Assertions.*;

class SizeTest {

    private static final Size size_min_max = Size.minMax(1, 3);
    private static final Size size_min_max_or_null = Size.minMaxOrNull(1, 3);
    private static final Size size_min = Size.min(1);
    private static final Size size_min_or_null = Size.minOrNull(1);
    private static final Size size_max = Size.max(3);
    private static final Size size_max_or_null = Size.maxOrNull(3);

    @Test
    void isSupportedString() {
        assertTrue(size_min_max.isSupportedType(String.class));
    }

    @Test
    void isSupportedArray() {
        assertTrue(size_min_max.isSupportedType(String[].class));
    }

    @Test
    void isSupportedList() {
        assertTrue(size_min_max.isSupportedType(List.class));
    }

    @Test
    void isSupportedMap() {
        assertTrue(size_min_max.isSupportedType(Map.class));
    }

    @Test
    void exceptionIfMinNegative() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Size.min(-1));
        assertEquals("Size min/max values must be >= 0 and min <= max", ex.getMessage());
    }

    @Test
    void exceptionIfMaxNegative() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Size.max(-1));
        assertEquals("Size min/max values must be >= 0 and min <= max", ex.getMessage());
    }

    @Test
    void exceptionIfMinGreaterMax() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Size.minMax(2, 1));
        assertEquals("Size min/max values must be >= 0 and min <= max", ex.getMessage());
    }

    @Test
    void minMaxSame() {
        assertDoesNotThrow(() -> Size.minMax(1, 1));
    }

    @Test
    void validateTrueString() {
        assertTrue(size_min_max.validate("abc", null));
    }

    @Test
    void validateTrueList() {
        assertTrue(size_min_max.validate(Arrays.asList("one", "two"), null));
    }

    @Test
    void validateTrueArray() {
        assertTrue(size_min_max.validate(new String[] { "foo" }, null));
    }

    @Test
    void validateTrueMap() {
        HashMap<Object, Object> hashMap = new HashMap<>();
        hashMap.put(1, "foo");
        assertTrue(size_min_max.validate(hashMap, null));
    }

    @Test
    void validateFalseString() {
        assertFalse(size_min_max.validate("abcd", null));
    }

    @Test
    void validateFalseList() {
        assertFalse(size_min_max.validate(List.of(), null));
    }

    @Test
    void validateFalseArray() {
        assertFalse(size_min_max.validate(new String[] {}, null));
    }

    @Test
    void validateFalseMap() {
        assertFalse(size_min_max.validate(new HashMap<>(), null));
    }

    @Test
    void validateNullValueToFalseMin() {
        assertFalse(size_min.validate(null, null));
    }

    @Test
    void validateNullValueToTrueMin() {
        assertTrue(size_min_or_null.validate(null, null));
    }

    @Test
    void validateNullValueToFalseMax() {
        assertFalse(size_max.validate(null, null));
    }

    @Test
    void validateNullValueToTrueMax() {
        assertTrue(size_max_or_null.validate(null, null));
    }

    @Test
    void validateNullValueToFalseMinMax() {
        assertFalse(size_min_max.validate(null, null));
    }

    @Test
    void validateNullValueToTrueMinMax() {
        assertTrue(size_min_max_or_null.validate(null, null));
    }

    @Test
    void serializeToJsonMin() {
        assertEquals("""
             "type":"SIZE","min":1""", size_min.serializeToJson());
    }

    @Test
    void serializeToJsonMinOrNull() {
        assertEquals("""
             "type":"SIZE","min":1,"nullEqualsTo":true""", size_min_or_null.serializeToJson());
    }

    @Test
    void serializeToJsonMax() {
        assertEquals("""
             "type":"SIZE","max":3""", size_max.serializeToJson());
    }

    @Test
    void serializeToJsonMaxOrNull() {
        assertEquals("""
             "type":"SIZE","max":3,"nullEqualsTo":true""", size_max_or_null.serializeToJson());
    }

    @Test
    void serializeToJsonMinMax() {
        assertEquals("""
             "type":"SIZE","min":1,"max":3""", size_min_max.serializeToJson());
    }

    @Test
    void serializeToJsonMinMaxOrNull() {
        assertEquals("""
             "type":"SIZE","min":1,"max":3,"nullEqualsTo":true""", size_min_max_or_null.serializeToJson());
    }

}

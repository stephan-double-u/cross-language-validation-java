package de.swa.clv.constraints;

import org.hamcrest.core.StringContains;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static de.swa.clv.test.Util.doubleQuote;
import static org.junit.Assert

.*;

public class SizeTest {

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    private static Size size_min_max = Size.minMax(1, 3);
    private static Size size_min = Size.min(1);
    private static Size size_max = Size.max(3);

    @Test
    public void isSupportedString() {
        assertTrue(size_min_max.isSupportedType(String.class));
    }

    @Test
    public void isSupportedArray() {
        assertTrue(size_min_max.isSupportedType(new String[]{}.getClass()));
    }

    @Test
    public void isSupportedList() {
        assertTrue(size_min_max.isSupportedType(List.class));
    }

    @Test
    public void isSupportedMap() {
        assertTrue(size_min_max.isSupportedType(Map.class));
    }

    @Test
    public void exceptionIfMinNagative() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Size min/max values must be >= 0 and min <= max"));
        Size.min(-1);
    }

    @Test
    public void exceptionIfMaxNagative() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Size min/max values must be >= 0 and min <= max"));
        Size.max(-1);
    }

    @Test
    public void exceptionIfMinGreaterMax() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Size min/max values must be >= 0 and min <= max"));
        Size.minMax(2, 1);
    }

    @Test
    public void minMaxSame() {
        Size.minMax(1, 1);
    }

    @Test
    public void validateTrueString() {
        assertTrue(size_min_max.validate("abc", null));
    }

    @Test
    public void validateTrueList() {
        assertTrue(size_min_max.validate(Arrays.asList("one", "two"), null));
    }

    @Test
    public void validateTrueArray() {
        assertTrue(size_min_max.validate(new String[] { "foo" }, null));
    }

    @Test
    public void validateTrueMap() {
        HashMap<Object, Object> hashMap = new HashMap<>();
        hashMap.put(1, "foo");
        assertTrue(size_min_max.validate(hashMap, null));
    }

    @Test
    public void validateFalseString() {
        assertFalse(size_min_max.validate("abcd", null));
    }

    @Test
    public void validateFalseList() {
        assertFalse(size_min_max.validate(Arrays.asList(), null));
    }

    @Test
    public void validateFalseArray() {
        assertFalse(size_min_max.validate(new String[] {}, null));
    }

    @Test
    public void validateFalseMap() {
        assertFalse(size_min_max.validate(new HashMap<>(), null));
    }

    @Test
    public void validateNull() {
        assertTrue(size_min_max.validate(null, null));
    }

    @Test
    public void serializeToJson_minmax() {
        assertEquals(doubleQuote("'type':'SIZE','min':1,'max':3"), size_min_max.serializeToJson());
    }

    @Test
    public void serializeToJson_min() {
        assertEquals(doubleQuote("'type':'SIZE','min':1"), size_min.serializeToJson());
    }

    @Test
    public void serializeToJson_max() {
        assertEquals(doubleQuote("'type':'SIZE','max':3"), size_max.serializeToJson());
    }
}

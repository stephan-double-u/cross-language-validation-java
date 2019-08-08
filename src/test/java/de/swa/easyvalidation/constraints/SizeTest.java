package de.swa.easyvalidation.constraints;

import org.junit.Test;

import java.util.Arrays;
import java.util.HashMap;

import static org.junit.Assert.*;

public class SizeTest {

    // TODO more and better tests ...

    static Size size_min_max = Size.minMax(1, 3);
    static Size size_min = Size.min(1);
    static Size size_max = Size.max(3);

    @Test
    public void min() {
    }

    @Test
    public void max() {
    }

    @Test
    public void minMax() {
    }

    @Test
    public void isSupportedType() {
    }

    @Test
    public void validateArgumentsOrFail() {
    }

    @Test
    public void validateString() {
        assertTrue(size_min_max.validate("abc", null));
    }

    @Test
    public void validateList() {
        assertTrue(size_min_max.validate(Arrays.asList("one", "two"), null));
    }

    @Test
    public void validateArray() {
        assertTrue(size_min_max.validate(new String[] { "foo" }, null));
    }

    @Test
    public void validateMap() {
        HashMap<Object, Object> hashMap = new HashMap<>();
        hashMap.put(1, "foo");
        assertTrue(size_min_max.validate(hashMap, null));
    }

    @Test
    public void validateNull() {
        assertTrue(size_min_max.validate(null, null));
    }

    @Test
    public void serializeToJson_minmax() {
        assertEquals("\"type\":\"SIZE\",\"min\":1,\"max\":3", size_min_max.serializeToJson());
    }

    @Test
    public void serializeToJson_min() {
        assertEquals("\"type\":\"SIZE\",\"min\":1", size_min.serializeToJson());
    }

    @Test
    public void serializeToJson_max() {
        assertEquals("\"type\":\"SIZE\",\"max\":3", size_max.serializeToJson());
    }
}

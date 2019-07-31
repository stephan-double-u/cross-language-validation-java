package de.swa.easyvalidation.constraints;

import org.junit.Test;
import org.junit.runner.RunWith;

import java.util.Arrays;
import java.util.HashMap;

import static org.junit.Assert.*;

public class SizeTest {

    // TODO more and better tests ...

    static Size size_1_3 = Size.minMax(1, 3);

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
        assertTrue(size_1_3.validate("abc", null));
    }

    @Test
    public void validateList() {
        assertTrue(size_1_3.validate(Arrays.asList("one", "two"), null));
    }

    @Test
    public void validateArray() {
        assertTrue(size_1_3.validate(new String[] { "foo" }, null));
    }

    @Test
    public void validateMap() {
        HashMap<Object, Object> hashMap = new HashMap<>();
        hashMap.put(1, "foo");
        assertTrue(size_1_3.validate(hashMap, null));
    }

    @Test
    public void validateNull() {
        assertTrue(size_1_3.validate(null, null));
    }

    @Test
    public void serializeToJson() {
        assertEquals("\"type\":\"SIZE\",\"min\":1,\"max\":3", size_1_3.serializeToJson());
    }
}

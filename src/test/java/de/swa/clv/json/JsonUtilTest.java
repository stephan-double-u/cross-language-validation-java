package de.swa.clv.json;

import org.junit.jupiter.api.Test;

import java.sql.JDBCType;
import java.time.LocalDate;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;

class JsonUtilTest {

    @Test
    void asObject() {
        assertEquals("{x}", JsonUtil.asObject("x"));
    }

    @Test
    void asKey() {
        assertEquals("\"x\":", JsonUtil.asKey("x"));
    }

    @Test
    void quoted() {
        assertEquals("\"x\"", JsonUtil.quoted("x"));
    }

    @Test
    void asArray() {
        assertEquals("[x]", JsonUtil.asArray("x"));
    }

    @Test
    void asArrayFromStrings() {
        assertEquals("[\"x\",\"y\"]", JsonUtil.asArray(Arrays.asList("x", "y")));
    }

    @Test
    void asArrayFromEnums() {
        assertEquals("[\"BLOB\"]", JsonUtil.asArray(Arrays.asList(JDBCType.BLOB)));
    }

    @Test
    void asArrayFromNumbers() {
        assertEquals("[1,2.0,3]", JsonUtil.asArray(Arrays.asList(1, 2d, 3L)));
    }

    @Test
    void asArrayFromDates() {
        assertEquals("[\"2019-12-31\"]", JsonUtil.asArray(Arrays.asList(LocalDate.of(2019, 12, 31))));
    }

}

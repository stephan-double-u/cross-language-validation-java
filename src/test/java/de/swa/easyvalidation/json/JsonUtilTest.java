package de.swa.easyvalidation.json;

import org.junit.Test;

import java.sql.JDBCType;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;

import static org.junit.Assert.*;

public class JsonUtilTest {

    @Test
    public void asObject() {
        assertEquals("{x}", JsonUtil.asObject("x"));
    }

    @Test
    public void asKey() {
        assertEquals("\"x\":", JsonUtil.asKey("x"));
    }

    @Test
    public void quoted() {
        assertEquals("\"x\"", JsonUtil.quoted("x"));
    }

    @Test
    public void asArray() {
        assertEquals("[x]", JsonUtil.asArray("x"));
    }

    @Test
    public void asArrayFromStrings() {
        assertEquals("[\"x\",\"y\"]", JsonUtil.asArray(Arrays.asList("x", "y")));
    }

    @Test
    public void asArrayFromEnums() {
        assertEquals("[\"BLOB\"]", JsonUtil.asArray(Arrays.asList(JDBCType.BLOB)));
    }

    @Test
    public void asArrayFromNumbers() {
        assertEquals("[1,2.0,3]", JsonUtil.asArray(Arrays.asList(1, 2d, 3L)));
    }

    @Test
    public void asArrayFromDates() {
        assertEquals("[\"2019-12-31\"]", JsonUtil.asArray(Arrays.asList(LocalDate.of(2019, 12, 31))));
    }

}
package de.swa.easyvalidation.constraints;

import org.junit.Test;

import java.sql.JDBCType;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class EqualsTest {

    // TODO more and better tests ...

    @Test
    public void tests() {
        EqualsAny any1 = Equals.any("foo", "bar", null);
        assertEquals("\"type\":\"EQUALS_ANY\",\"values\":[\"foo\",\"bar\",null]", any1.serializeToJson());
        assertTrue(any1.validate(null, null));
        assertTrue(any1.validate("bar", null));

        EqualsAny any2 = Equals.any(JDBCType.BLOB, JDBCType.DOUBLE, null);
        assertEquals("\"type\":\"EQUALS_ANY\",\"values\":[\"BLOB\",\"DOUBLE\",null]", any2.serializeToJson());
        assertTrue(any2.validate(JDBCType.BLOB, null));

        EqualsNone none1 = Equals.none("foo", "bar", null);
        assertEquals("\"type\":\"EQUALS_NONE\",\"values\":[\"foo\",\"bar\",null]", none1.serializeToJson());
        assertFalse(none1.validate(null, null));
        assertFalse(none1.validate("bar", null));
        assertTrue(none1.validate("zoo", null));

        EqualsNone none2 = Equals.none(JDBCType.BLOB, JDBCType.DOUBLE, null);
        assertEquals("\"type\":\"EQUALS_NONE\",\"values\":[\"BLOB\",\"DOUBLE\",null]", none2.serializeToJson());
        assertFalse(none2.validate(JDBCType.BLOB, null));
        assertTrue(none2.validate(JDBCType.VARCHAR, null));
    }
}

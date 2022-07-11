package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.hamcrest.core.StringContains;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.sql.JDBCType;

import static org.junit.Assert

.assertEquals;
import static org.junit.Assert

.assertFalse;
import static org.junit.Assert

.assertTrue;

public class EqualsNoneTest {

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Test
    public void validateStringVsString() {
        EqualsNone none = Equals.none("foo", "bar");
        assertFalse(none.validate("bar", null));
    }

    @Test
    public void validateNullVsNull() {
        EqualsNone none = Equals.none("foo", "bar");
        assertTrue(none.validate(null, null));
    }

    @Test
    public void validateStringVsEnum() {
        EqualsNone none = Equals.none("BLOB");
        assertFalse(none.validate(JDBCType.BLOB, null));
    }

    @Test
    public void validateEnumVsString() {
        EqualsNone none = Equals.none(JDBCType.BLOB);
        assertFalse(none.validate("BLOB", null));
    }

    @Test
    public void validateEnumVsOtherEnum() {
        EqualsNone none = Equals.none(JDBCType.BLOB);
        assertFalse(none.validate(OtherEnum.BLOB, null));
    }

    @Test
    public void validateNumber() {
        EqualsNone none = Equals.none(1L, 2L, 3L);
        assertFalse(none.validate(2L, null));
    }

    @Test
    public void validateBoolean() {
        EqualsNone none = Equals.none(Boolean.TRUE);
        assertFalse(none.validate(Boolean.TRUE, null));
    }

    @Test
    public void serializeString() {
        EqualsNone none = Equals.none("foo", "bar");
        assertEquals(Util.doubleQuote("'type':'EQUALS_NONE','values':['foo','bar']"), none.serializeToJson());
    }

    @Test
    public void serializeEnum() {
        EqualsNone none = Equals.none(JDBCType.BLOB, OtherEnum.PLOP);
        assertEquals(Util.doubleQuote("'type':'EQUALS_NONE','values':['BLOB','PLOP']"), none.serializeToJson());
    }

    @Test
    public void serializeNumber() {
        EqualsNone none = Equals.none(1, 2, 3);
        assertEquals(Util.doubleQuote("'type':'EQUALS_NONE','values':[1,2,3]"), none.serializeToJson());
    }

    @Test
    public void serializeBoolean() {
        EqualsNone none = Equals.none(Boolean.FALSE);
        assertEquals(Util.doubleQuote("'type':'EQUALS_NONE','values':[false]"), none.serializeToJson());
    }

    enum OtherEnum {
        BLOB, PLOP
    }
}

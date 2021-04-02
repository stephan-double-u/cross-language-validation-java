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
    public void exceptionIfStringIsNull() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Null values are not allowed"));
        Equals.none((String) null);
    }

    @Test
    public void exceptionIfEnumIsNull() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Null values are not allowed"));
        Equals.none((Enum) null);
    }

    @Test
    public void exceptionIfNumberIsNull() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Null values are not allowed"));
        Equals.none((Number) null);
    }

    @Test
    public void exceptionIfBooleanIsNull() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Null values are not allowed"));
        Equals.none( (Boolean) null);
    }

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
        EqualsNone none = Equals.none(1, 2L, 3d);
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
        EqualsNone none = Equals.none(1, 2L, 3d);
        assertEquals(Util.doubleQuote("'type':'EQUALS_NONE','values':[1,2,3.0]"), none.serializeToJson());
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

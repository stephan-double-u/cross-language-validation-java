package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.hamcrest.core.StringContains;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.sql.Date;
import java.sql.JDBCType;
import java.time.LocalDate;

import static org.junit.Assert.*;

public class EqualsAnyTest {

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Test
    public void exceptionIfStringIsNull() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Null values are not allowed"));
        Equals.any((String) null);
    }

    @Test
    public void exceptionIfEnumIsNull() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Null values are not allowed"));
        EqualsAny any = Equals.any((Enum) null);
    }

    @Test
    public void exceptionIfNumberIsNull() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Null values are not allowed"));
        EqualsAny any = Equals.any((Number) null);
    }

    @Test
    public void exceptionIfBooleanIsNull() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Null values are not allowed"));
        EqualsAny any = Equals.any( (Boolean) null);
    }

    @Test
    public void exceptionIfLocalDateIsNull() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Null values are not allowed"));
        EqualsAny any = Equals.any( (LocalDate) null);
    }

    @Test
    public void validateStringVsString() {
        EqualsAny any = Equals.any("foo", "bar");
        assertTrue(any.validate("bar", null));
    }

    @Test
    public void validateNullVsNull() {
        EqualsAny any = Equals.any("foo", "bar");
        assertFalse(any.validate(null, null));
    }

    @Test
    public void validateStringVsEnum() {
        EqualsAny any = Equals.any("BLOB");
        assertTrue(any.validate(JDBCType.BLOB, null));
    }

    @Test
    public void validateEnumVsString() {
        EqualsAny any = Equals.any(JDBCType.BLOB);
        assertTrue(any.validate("BLOB", null));
    }

    @Test
    public void validateEnumVsOtherEnum() {
        EqualsAny any = Equals.any(JDBCType.BLOB);
        assertTrue(any.validate(OtherEnum.BLOB, null));
    }

    @Test
    public void validateNumber() {
        EqualsAny any = Equals.any(1, 2L, 3d);
        assertTrue(any.validate(2L, null));
    }

    @Test
    public void validateBoolean() {
        EqualsAny any = Equals.any(Boolean.TRUE);
        assertTrue(any.validate(Boolean.TRUE, null));
    }

    @Test
    public void validateLocalDate() {
        final LocalDate localDate1 = LocalDate.of(2019, 12, 31);
        final LocalDate localDate2 = LocalDate.of(2019, 12, 31);
        EqualsAny any = Equals.any(localDate1);
        assertTrue(any.validate(localDate2, null));
    }

    @Test
    public void validateLocalDateFalse() {
        final LocalDate localDate1 = LocalDate.of(2019, 12, 30);
        final LocalDate localDate2 = LocalDate.of(2019, 12, 31);
        EqualsAny any = Equals.any(localDate1);
        assertFalse(any.validate(localDate2, null));
    }

    @Test
    public void serializeString() {
        EqualsAny any = Equals.any("foo", "bar");
        assertEquals(Util.doubleQuote("'type':'EQUALS_ANY','values':['foo','bar']"), any.serializeToJson());
    }

    @Test
    public void serializeEnum() {
        EqualsAny any = Equals.any(JDBCType.BLOB, OtherEnum.PLOP);
        assertEquals(Util.doubleQuote("'type':'EQUALS_ANY','values':['BLOB','PLOP']"), any.serializeToJson());
    }

    @Test
    public void serializeNumber() {
        EqualsAny any = Equals.any(1, 2L, 3d);
        assertEquals(Util.doubleQuote("'type':'EQUALS_ANY','values':[1,2,3.0]"), any.serializeToJson());
    }

    @Test
    public void serializeBoolean() {
        EqualsAny any = Equals.any(Boolean.FALSE);
        assertEquals(Util.doubleQuote("'type':'EQUALS_ANY','values':[false]"), any.serializeToJson());
    }

    @Test
    public void serializeLocalDate() {
        EqualsAny any = Equals.any(LocalDate.of(2019, 12, 31));
        assertEquals(Util.doubleQuote("'type':'EQUALS_ANY','values':['2019-12-31']"), any.serializeToJson());
    }

    enum OtherEnum {
        BLOB, PLOP
    }
}

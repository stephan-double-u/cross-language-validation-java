package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.hamcrest.core.StringContains;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Date;
import java.sql.JDBCType;
import java.time.LocalDate;
import java.util.Arrays;

import static org.junit.Assert.*;

public class EqualsAnyTest {

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Test
    public void validateStringVsString() {
        EqualsAny any = Equals.any("foo", "bar");
        assertTrue(any.validate("bar", null));
    }

    @Test
    public void validateNullStringVsNu__() {
        EqualsAny any = Equals.any((String) null);
        assertTrue(any.validate(null, null));
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
    public void validateInteger() {
        EqualsAny any = Equals.any(1L, 2L, 3L);
        assertTrue(any.validate(2L, null));
    }

    @Test
    public void validateBigInteger() {
        EqualsAny any = Equals.any(BigInteger.ONE, BigInteger.TEN);
        assertTrue(any.validate(BigInteger.TEN, null));
    }

    @Test
    public void validateBigDecimal() {
        EqualsAny any = Equals.any(BigDecimal.valueOf(1.23), BigDecimal.valueOf(4.56));
        assertTrue(any.validate(BigDecimal.valueOf(4.56), null));
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
        EqualsAny any = Equals.any(1, 2, 3);
        assertEquals(Util.doubleQuote("'type':'EQUALS_ANY','values':[1,2,3]"), any.serializeToJson());
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

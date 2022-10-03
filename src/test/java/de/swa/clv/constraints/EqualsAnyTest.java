package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.JDBCType;
import java.time.LocalDate;

import static de.swa.clv.constraints.ConstraintRoot.EMPTY_VALUES_ERR_MESSAGE;
import static de.swa.clv.constraints.ConstraintRoot.NULL_VALUE_ERR_MESSAGE;
import static org.junit.jupiter.api.Assertions.*;

public class EqualsAnyTest {

    @Test
    public void nullValuesNotAllowed() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Equals.any((String) null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    public void emptyValuesNotAllowed() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Equals.any(new String[] {}));
        assertEquals(EMPTY_VALUES_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    public void validateAnyStringVsString() {
        EqualsAny any = Equals.any("foo", "bar");
        assertTrue(any.validate("bar", null));
    }

    @Test
    public void validateAnyStringVsStringFail() {
        EqualsAny any = Equals.any("foo", "bar");
        assertFalse(any.validate("zoo", null));
    }

    @Test
    public void validateAnyStringVsNull() {
        EqualsAny any = Equals.any("foo", "bar");
        assertFalse(any.validate(null, null));
    }

    @Test
    public void validateAnyOrNullStringVsNull() {
        EqualsAny anyOrNull = Equals.anyOrNull("foo", "bar");
        assertTrue(anyOrNull.validate(null, null));
    }

    @Test
    public void validateAnyOrNullStringVsString() {
        EqualsAny anyOrNull = Equals.anyOrNull("foo", "bar");
        assertTrue(anyOrNull.validate("bar", null));
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
    public void validateAnyOrNullEnumVsOtherEnum() {
        EqualsAny anyOrNull = Equals.anyOrNull(JDBCType.BLOB);
        assertTrue(anyOrNull.validate(OtherEnum.BLOB, null));
    }

    @Test
    public void validateAnyOrNullEnumVsOtherEnumFalse() {
        EqualsAny anyOrNull = Equals.anyOrNull(JDBCType.BLOB);
        assertFalse(anyOrNull.validate(JDBCType.DATE, null));
    }

    @Test
    public void validateAnyOrNullEnumVsNull() {
        EqualsAny anyOrNull = Equals.anyOrNull(JDBCType.BLOB);
        assertTrue(anyOrNull.validate(null, null));
    }

    @Test
    public void validateInteger() {
        EqualsAny any = Equals.any(1L, 2L, 3L);
        assertTrue(any.validate(2L, null));
    }

    @Test
    public void validateIntegerFalse() {
        EqualsAny any = Equals.any(1L, 2L, 3L);
        assertFalse(any.validate(4L, null));
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
    public void validateAnyOrNullVsNumber() {
        EqualsAny anyOrNull = Equals.anyOrNull(1);
        assertTrue(anyOrNull.validate(null, null));
    }

    @Test
    public void validateBoolean() {
        EqualsAny any = Equals.any(Boolean.TRUE);
        assertTrue(any.validate(Boolean.TRUE, null));
    }

    @Test
    public void validateBooleanFalse() {
        EqualsAny any = Equals.any(Boolean.TRUE);
        assertFalse(any.validate(Boolean.FALSE, null));
    }

    @Test
    public void validateAnyOrNullBoolean() {
        EqualsAny anyOrNull = Equals.anyOrNull(Boolean.TRUE);
        assertTrue(anyOrNull.validate(null, null));
    }

    @Test
    public void validateLocalDateTrue() {
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
    public void validateAnyOrNullLocalDateTrue() {
        EqualsAny anyOrNull = Equals.anyOrNull(LocalDate.now());
        assertTrue(anyOrNull.validate(null, null));
    }

    @Test
    public void serializeString() {
        EqualsAny any = Equals.any("foo", "bar");
        assertEquals(Util.doubleQuote("'type':'EQUALS_ANY','values':['foo','bar']"), any.serializeToJson());
    }

    @Test
    public void serializeStringNullEqualsTrue() {
        EqualsAny anyOrNull = Equals.anyOrNull("foo", "bar");
        assertEquals("""
                "type":"EQUALS_ANY","values":["foo","bar"],"nullEqualsTo":true""", anyOrNull.serializeToJson());
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

package de.swa.clv.constraints;

import org.junit.jupiter.api.Test;

import java.sql.JDBCType;
import java.time.LocalDate;

import static de.swa.clv.constraints.Constraint.NULL_VALUE_ERR_MESSAGE;
import static org.junit.jupiter.api.Assertions.*;

class EqualsNoneTest {

    @Test
    void nullValuesNotAllowed() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Equals.none((String) null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void validateNoneStringVsStringTrue() {
        EqualsNone none = Equals.none("foo", "bar");
        assertTrue(none.validate("zoo", null));
    }

    @Test
    void validateNoneStringVsStringFalse() {
        EqualsNone none = Equals.none("foo", "bar");
        assertFalse(none.validate("bar", null));
    }

    @Test
    void validateNoneStringVsNullTrue() {
        EqualsNone none = Equals.none("foo", "bar");
        assertTrue(none.validate(null, null));
    }

    @Test
    void validateNoneNorNullStringVsStringTrue() {
        EqualsNone noneNorNull = Equals.noneNorNull("foo", "bar");
        assertTrue(noneNorNull.validate("zoo", null));
    }

    @Test
    void validateNoneNorNullStringVsStringFalse() {
        EqualsNone noneNorNull = Equals.noneNorNull("foo", "bar");
        assertFalse(noneNorNull.validate("bar", null));
    }

    @Test
    void validateNoneNorNullStringVsNull() {
        EqualsNone noneNorNull = Equals.noneNorNull("foo", "bar");
        assertFalse(noneNorNull.validate(null, null));
    }

    @Test
    void validateNoneStringVsEnumTrue() {
        EqualsNone none = Equals.none("BLOB");
        assertTrue(none.validate(JDBCType.DATE, null));
    }

    @Test
    void validateNoneStringVsEnumFalse() {
        EqualsNone none = Equals.none("BLOB");
        assertFalse(none.validate(JDBCType.BLOB, null));
    }

    @Test
    void validateNoneNorNullStringVsEnum() {
        EqualsNone noneNorNull = Equals.noneNorNull("BLOB");
        assertFalse(noneNorNull.validate(JDBCType.BLOB, null));
    }

    @Test
    void validateEnumVsString() {
        EqualsNone none = Equals.none(JDBCType.BLOB);
        assertFalse(none.validate("BLOB", null));
    }

    @Test
    void validateEnumVsOtherEnum() {
        EqualsNone none = Equals.none(JDBCType.BLOB);
        assertFalse(none.validate(OtherEnum.BLOB, null));
    }

    @Test
    void validateNoneNorNullEnumVsNull() {
        EqualsNone noneNorNull = Equals.noneNorNull(JDBCType.BLOB);
        assertFalse(noneNorNull.validate(null, null));
    }

    @Test
    void validateNumberTrue() {
        EqualsNone none = Equals.none(1L, 2L, 3L);
        assertTrue(none.validate(4L, null));
    }

    @Test
    void validateNumberFalse() {
        EqualsNone none = Equals.none(1L, 2L, 3L);
        assertFalse(none.validate(2L, null));
    }

    @Test
    void validateNoneNorNullNumber() {
        EqualsNone noneNorNull = Equals.noneNorNull(1L, 2L, 3L);
        assertFalse(noneNorNull.validate(null, null));
    }

    @Test
    void validateBooleanFalse() {
        EqualsNone none = Equals.none(Boolean.TRUE);
        assertFalse(none.validate(Boolean.TRUE, null));
    }

    @Test
    void validateBooleanTrue() {
        EqualsNone none = Equals.none(Boolean.TRUE);
        assertTrue(none.validate(Boolean.FALSE, null));
    }

    @Test
    void validateNoneNorNullBoolean() {
        EqualsNone noneNorNull = Equals.noneNorNull(Boolean.TRUE);
        assertFalse(noneNorNull.validate(null, null));
    }

    @Test
    void validateLocalDateFalse() {
        final LocalDate localDate1 = LocalDate.of(2019, 12, 31);
        final LocalDate localDate2 = LocalDate.of(2019, 12, 31);
        EqualsNone none = Equals.none(localDate1);
        assertFalse(none.validate(localDate2, null));
    }

    @Test
    void validateLocalDateTrue() {
        final LocalDate localDate1 = LocalDate.of(2019, 12, 30);
        final LocalDate localDate2 = LocalDate.of(2019, 12, 31);
        EqualsNone none = Equals.none(localDate1);
        assertTrue(none.validate(localDate2, null));
    }

    @Test
    void validateAnyOrNullLocalDateFalse() {
        EqualsNone noneNorNull = Equals.noneNorNull(LocalDate.now());
        assertFalse(noneNorNull.validate(null, null));
    }

    @Test
    void serializeString() {
        EqualsNone none = Equals.none("foo", "bar");
        assertEquals("""
                "type":"EQUALS_NONE","values":["foo","bar"]""", none.serializeToJson());
    }

    @Test
    void serializeStringNorNull() {
        EqualsNone noneNorNull = Equals.noneNorNull("foo", "bar");
        assertEquals("""
                "type":"EQUALS_NONE","values":["foo","bar"],"nullEqualsTo":false""", noneNorNull.serializeToJson());
    }

    @Test
    void serializeEnum() {
        EqualsNone none = Equals.none(JDBCType.BLOB, OtherEnum.PLOP);
        assertEquals("""
                "type":"EQUALS_NONE","values":["BLOB","PLOP"]""", none.serializeToJson());
    }

    @Test
    void serializeNumber() {
        EqualsNone none = Equals.none(1, 2, 3);
        assertEquals("""
                "type":"EQUALS_NONE","values":[1,2,3]""", none.serializeToJson());
    }

    @Test
    void serializeBoolean() {
        EqualsNone none = Equals.none(Boolean.FALSE);
        assertEquals("""
                "type":"EQUALS_NONE","values":[false]""", none.serializeToJson());
    }

    enum OtherEnum {
        BLOB, PLOP
    }
}

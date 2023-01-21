package de.swa.clv.constraints;

import de.swa.clv.Validator;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import static de.swa.clv.constraints.Constraint.EMPTY_VALUES_ERR_MESSAGE;
import static de.swa.clv.constraints.Constraint.NULL_VALUE_ERR_MESSAGE;
import static de.swa.clv.constraints.Quarter.QUARTERS_ERR_MESSAGE;
import static org.junit.jupiter.api.Assertions.*;

class QuarterAnyRefTest {

    public record Foo(long quarter, List<Long> integerList, String aString) {}
    static Foo foo = new Foo(1, List.of(-1L, 42L, 666L, 1L), "bar");

    @Test
    void anyValuesMustNotBeNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                Quarter::anyRef);
        assertEquals(EMPTY_VALUES_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void anyValuesMustNotContainNulls() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Quarter.anyRef("", null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void anyOrNullValuesMustNotContainNulls() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Quarter.anyRefOrNull("", null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void unsupportedType() {
        QuarterAnyRef anyRef = Quarter.anyRef("quarter");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("quarter", Foo.class);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> anyRef.validate("stringIsUnsupported", foo));
        assertEquals("Unsupported type: class java.lang.String", ex.getMessage());
    }

    @Test
    void unsupportedReferencedType() {
        QuarterAnyRef anyRef = Quarter.anyRef("aString");
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> anyRef.validateValuesOrFail(foo.getClass(), null));
        assertEquals("Type of referenced property is class java.lang.String but must be class java.lang.Number",
                ex.getMessage());
    }

    @Test
    void validateNullObjectToFalse() {
        QuarterAnyRef any = Quarter.anyRef("whatever");
        assertFalse(any.validate(null, null));
    }

    @Test
    void validateNullObjectToTrue() {
        QuarterAnyRef anyRef = Quarter.anyRefOrNull("whatever");
        assertTrue(anyRef.validate(null, null));
    }

    @Test
    void validateLocalDateToTrue() {
        QuarterAnyRef anyRef = Quarter.anyRef("integerList[*]", "quarter");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("quarter", Foo.class);
        assertTrue(anyRef.validate(LocalDate.of(2022, 1, 1), foo));
    }

    @Test
    void validateLocalDateToFalse() {
        QuarterAnyRef anyRef = Quarter.anyRef("integerList[0-2]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("quarter", Foo.class);
        assertFalse(anyRef.validate(LocalDate.of(2022, 1, 1), foo));
    }

    @Test
    void validateLocalDateTimeToTrue() {
        QuarterAnyRef anyRef = Quarter.anyRef("integerList[*]", "quarter");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("quarter", Foo.class);
        assertTrue(anyRef.validate(LocalDateTime.of(2022, 1, 1, 0, 0), foo));
    }

    @Test
    void validateLocalDateTimeToFalse() {
        QuarterAnyRef anyRef = Quarter.anyRef("integerList[0-2]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("quarter", Foo.class);
        assertFalse(anyRef.validate(LocalDateTime.of(2022, 1, 1, 0, 0), foo));
    }

    @Test
    void validateCalenderToTrue() {
        QuarterAnyRef anyRef = Quarter.anyRef("integerList[*]", "quarter");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("quarter", Foo.class);
        Calendar cal = Calendar.getInstance();
        cal.set(2022, Calendar.JANUARY, 1);
        assertTrue(anyRef.validate(cal, foo));
    }

    @Test
    void validateCalenderToFalse() {
        QuarterAnyRef anyRef = Quarter.anyRef("integerList[0-2]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("quarter", Foo.class);
        Calendar cal = Calendar.getInstance();
        cal.set(2022, Calendar.JANUARY, 1);
        assertFalse(anyRef.validate(cal, foo));
    }

    @Test
    void validateUtilDateToTrue() {
        QuarterAnyRef anyRef = Quarter.anyRef("integerList[*]", "quarter");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("quarter", Foo.class);
        Date date = Date.from(LocalDate.of(2022, 1, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
        assertTrue(anyRef.validate(date, foo));
    }

    @Test
    void validateUtilDateToFalse() {
        QuarterAnyRef anyRef = Quarter.anyRef("integerList[0-2]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("quarter", Foo.class);
        Date date = Date.from(LocalDate.of(2022, 1, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
        assertFalse(anyRef.validate(date, foo));
    }

    @Test
    void serializeQuartersAnyRef() {
        QuarterAnyRef anyRef = Quarter.anyRef("foo", "bar");
        assertEquals("""
                "type":"QUARTER_ANY_REF","values":["foo","bar"]""",
                anyRef.serializeToJson());
    }

    @Test
    void serializeQuartersAnyRefOrNull() {
        QuarterAnyRef anyRef = Quarter.anyRefOrNull("foo", "bar");
        assertEquals("""
                "type":"QUARTER_ANY_REF","values":["foo","bar"],"nullEqualsTo":true""",
                anyRef.serializeToJson());
    }

    @Test
    void serializeQuartersAnyRef_ofUpdate() {
        QuarterAnyRef anyRef = Quarter.anyRef("foo", "bar").ofUpdate();
        assertEquals("""
                "type":"QUARTER_ANY_REF","values":["foo","bar"],"refTarget":"UPDATE_ENTITY\"""",
                anyRef.serializeToJson());
    }

    @Test
    void serializeQuartersAnyRef_ofCurrent() {
        QuarterAnyRef anyRef = Quarter.anyRef("foo", "bar").ofCurrent();
        assertEquals("""
                "type":"QUARTER_ANY_REF","values":["foo","bar"],"refTarget":"CURRENT_ENTITY\"""",
                anyRef.serializeToJson());
    }

}

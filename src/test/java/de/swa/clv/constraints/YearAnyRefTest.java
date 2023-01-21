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
import static org.junit.jupiter.api.Assertions.*;

class YearAnyRefTest {

    public record Foo(int year, List<Integer> integerList, String aString) {}
    static Foo foo = new Foo(2000, List.of(2010, 2020, 2030), "bar");

    @Test
    void anyValuesMustNotBeNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                Year::anyRef);
        assertEquals(EMPTY_VALUES_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void anyValuesMustNotContainNulls() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Year.anyRef("", null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void anyOrNullValuesMustNotContainNulls() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Year.anyRefOrNull("", null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void unsupportedType() {
        YearAnyRef anyRef = Year.anyRef("year");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("year", Foo.class);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> anyRef.validate("stringIsUnsupported", foo));
        assertEquals("Unsupported type: class java.lang.String", ex.getMessage());
    }

    @Test
    void unsupportedReferencedType() {
        YearAnyRef anyRef = Year.anyRef("aString");
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> anyRef.validateValuesOrFail(foo.getClass(), null));
        assertEquals("Type of referenced property is class java.lang.String but must be class java.lang.Integer",
                ex.getMessage());
    }

    @Test
    void validateNullObjectToFalse() {
        YearAnyRef any = Year.anyRef("whatever");
        assertFalse(any.validate(null, null));
    }

    @Test
    void validateNullObjectToTrue() {
        YearAnyRef anyRef = Year.anyRefOrNull("whatever");
        assertTrue(anyRef.validate(null, null));
    }

    @Test
    void validateLocalDateToTrue() {
        YearAnyRef anyRef = Year.anyRef("integerList[*]", "year");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("year", Foo.class);
        assertTrue(anyRef.validate(LocalDate.of(2000, 1, 1), foo));
    }

    @Test
    void validateLocalDateToFalse() {
        YearAnyRef anyRef = Year.anyRef("integerList[0-2]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("year", Foo.class);
        assertFalse(anyRef.validate(LocalDate.of(2000, 1, 1), foo));
    }

    @Test
    void validateLocalDateTimeToTrue() {
        YearAnyRef anyRef = Year.anyRef("integerList[*]", "year");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("year", Foo.class);
        assertTrue(anyRef.validate(LocalDateTime.of(2020, 1, 1, 0, 0), foo));
    }

    @Test
    void validateLocalDateTimeToFalse() {
        YearAnyRef anyRef = Year.anyRef("integerList[0-2]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("year", Foo.class);
        assertFalse(anyRef.validate(LocalDateTime.of(2000, 1, 1, 0, 0), foo));
    }

    @Test
    void validateCalenderToTrue() {
        YearAnyRef anyRef = Year.anyRef("integerList[*]", "year");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("year", Foo.class);
        Calendar cal = Calendar.getInstance();
        cal.set(2020, Calendar.JANUARY, 1);
        assertTrue(anyRef.validate(cal, foo));
    }

    @Test
    void validateCalenderToFalse() {
        YearAnyRef anyRef = Year.anyRef("integerList[0-2]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("year", Foo.class);
        Calendar cal = Calendar.getInstance();
        cal.set(2000, Calendar.JANUARY, 1);
        assertFalse(anyRef.validate(cal, foo));
    }

    @Test
    void validateUtilDateToTrue() {
        YearAnyRef anyRef = Year.anyRef("integerList[*]", "year");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("year", Foo.class);
        Date date = Date.from(LocalDate.of(2020, 1, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
        assertTrue(anyRef.validate(date, foo));
    }

    @Test
    void validateUtilDateToFalse() {
        YearAnyRef anyRef = Year.anyRef("integerList[0-2]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("year", Foo.class);
        Date date = Date.from(LocalDate.of(2000, 1, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
        assertFalse(anyRef.validate(date, foo));
    }

    @Test
    void serializeYearsAnyRef() {
        YearAnyRef anyRef = Year.anyRef("foo", "bar");
        assertEquals("""
                "type":"YEAR_ANY_REF","values":["foo","bar"]""",
                anyRef.serializeToJson());
    }

    @Test
    void serializeYearsAnyRefOrNull() {
        YearAnyRef anyRef = Year.anyRefOrNull("foo", "bar");
        assertEquals("""
                "type":"YEAR_ANY_REF","values":["foo","bar"],"nullEqualsTo":true""",
                anyRef.serializeToJson());
    }

    @Test
    void serializeYearsAnyRef_ofUpdate() {
        YearAnyRef anyRef = Year.anyRef("foo", "bar").ofUpdate();
        assertEquals("""
                "type":"YEAR_ANY_REF","values":["foo","bar"],"refTarget":"UPDATE_ENTITY\"""",
                anyRef.serializeToJson());
    }

    @Test
    void serializeYearsAnyRef_ofCurrent() {
        YearAnyRef anyRef = Year.anyRef("foo", "bar").ofCurrent();
        assertEquals("""
                "type":"YEAR_ANY_REF","values":["foo","bar"],"refTarget":"CURRENT_ENTITY\"""",
                anyRef.serializeToJson());
    }

}

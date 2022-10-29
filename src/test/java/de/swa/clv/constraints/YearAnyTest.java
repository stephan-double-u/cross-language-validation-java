package de.swa.clv.constraints;

import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Date;

import static de.swa.clv.constraints.Constraint.EMPTY_VALUES_ERR_MESSAGE;
import static de.swa.clv.constraints.Constraint.NULL_VALUE_ERR_MESSAGE;
import static org.junit.jupiter.api.Assertions.*;

public class YearAnyTest {

    @Test
    public void anyValuesMustNotBeNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                Year::any);
        assertEquals(EMPTY_VALUES_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    public void anyValuesMustNotContainNulls() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Year.any(1, null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    public void anyOrNullValuesMustNotContainNulls() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Year.anyOrNull(1, null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    public void unsupportedType() {
        YearAny any = Year.any(1);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> any.validate("string is unsupported", null));
        assertEquals("Unsupported type: class java.lang.String", ex.getMessage());
    }

    @Test
    public void validateNullObjectToFalse() {
        YearAny any = Year.any(1);
        assertFalse(any.validate(null, null));
    }

    @Test
    public void validateNullObjectToTrue() {
        YearAny any = Year.anyOrNull(1);
        assertTrue(any.validate(null, null));
    }

    @Test
    public void validateLocalDateToTrue() {
        YearAny any = Year.any(2022);
        assertTrue(any.validate(LocalDate.of(2022, 1, 1), null));
    }

    @Test
    public void validateLocalDateToFalse() {
        YearAny any = Year.any(2021, 2023);
        assertFalse(any.validate(LocalDate.of(2022, 1, 1), null));
    }

    @Test
    public void validateLocalDateTimeToTrue() {
        YearAny any = Year.any(2022);
        assertTrue(any.validate(LocalDateTime.of(2022, 1, 1, 20, 15), null));
    }

    @Test
    public void validateLocalDateTimeToFalse() {
        YearAny any = Year.any(1999);
        assertFalse(any.validate(LocalDateTime.of(2022, 1, 1, 20, 15), null));
    }

    @Test
    public void validateCalendarToTrue() {
        YearAny any = Year.any(2022);
        Calendar cal = Calendar.getInstance();
        cal.set(2022, Calendar.JANUARY, 1);
        assertTrue(any.validate(cal, null));
    }

    @Test
    public void validateCalendarToFalse() {
        YearAny any = Year.any(1999);
        Calendar cal = Calendar.getInstance();
        cal.set(2022, Calendar.JANUARY, 1);
        assertFalse(any.validate(cal, null));
    }

    @Test
    public void validateUtilDateToTrue() {
        YearAny any = Year.any(2022);
        Date date = Date.from(LocalDate.of(2022, 1, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
        assertTrue(any.validate(date, null));
    }

    @Test
    public void validateUtilDateToFalse() {
        YearAny any = Year.any(1999);
        Date date = Date.from(LocalDate.of(2022, 1, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
        assertFalse(any.validate(date, null));
    }

    @Test
    public void serializeYearsAny() {
        YearAny any = Year.any(1000,2000);
        assertEquals("""
                "type":"YEAR_ANY","values":[1000,2000]""",
                any.serializeToJson());
    }

    @Test
    public void serializeYearsAnyOrNull() {
        YearAny any = Year.anyOrNull(1000, 2000);
        assertEquals("""
                "type":"YEAR_ANY","values":[1000,2000],"nullEqualsTo":true""",
                any.serializeToJson());
    }

}

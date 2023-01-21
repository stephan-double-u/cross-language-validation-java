package de.swa.clv.constraints;

import org.junit.jupiter.api.Test;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Date;

import static de.swa.clv.constraints.Constraint.EMPTY_VALUES_ERR_MESSAGE;
import static de.swa.clv.constraints.Constraint.NULL_VALUE_ERR_MESSAGE;
import static java.time.DayOfWeek.*;
import static java.time.DayOfWeek.MONDAY;
import static org.junit.jupiter.api.Assertions.*;

class WeekdayTest {

    @Test
    void anyValuesMustNotBeNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                Weekday::any);
        assertEquals(EMPTY_VALUES_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void anyValuesMustNotContainNulls() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Weekday.any(SUNDAY, null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void anyOrNullValuesMustNotContainNulls() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Weekday.anyOrNull(SUNDAY, null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void unsupportedType() {
        Dates weekday = Weekday.any(SUNDAY);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> weekday.validate("string is unsupported", null));
        assertEquals("Unsupported type: class java.lang.String", ex.getMessage());
    }

    @Test
    void validateNullObjectToFalse() {
        Weekday weekday = Weekday.any(SUNDAY);
        assertFalse(weekday.validate(null, null));
    }

    @Test
    void validateNullObjectToTrue() {
        Weekday weekday = Weekday.anyOrNull(SUNDAY);
        assertTrue(weekday.validate(null, null));
    }

    @Test
    void validateLocalDateToTrue() {
        Weekday weekday = Weekday.any(SATURDAY);
        assertTrue(weekday.validate(LocalDate.of(2022, 1, 1), null));
    }

    @Test
    void validateLocalDateToFalse() {
        Weekday weekday = Weekday.any(MONDAY, THURSDAY, WEDNESDAY, THURSDAY, FRIDAY, SUNDAY);
        assertFalse(weekday.validate(LocalDate.of(2022, 1, 1), null));
    }

    @Test
    void validateLocalDateTimeToTrue() {
        Weekday weekday = Weekday.any(SATURDAY);
        assertTrue(weekday.validate(LocalDateTime.of(2022, 1, 1, 20, 15), null));
    }

    @Test
    void validateLocalDateTimeToFalse() {
        Weekday weekday = Weekday.any(MONDAY, THURSDAY, WEDNESDAY, THURSDAY, FRIDAY, SUNDAY);
        assertFalse(weekday.validate(LocalDateTime.of(2022, 1, 1, 20, 15), null));
    }

    @Test
    void validateCalendarToTrue() {
        Weekday weekday = Weekday.any(DayOfWeek.values());
        assertTrue(weekday.validate(Calendar.getInstance(), null));
    }

    @Test
    void validateCalendarToFalse() {
        Weekday weekday = Weekday.any(MONDAY, THURSDAY, WEDNESDAY, THURSDAY, FRIDAY, SUNDAY);
        Calendar cal = Calendar.getInstance();
        cal.set(2022, Calendar.JANUARY, 1);
        assertFalse(weekday.validate(cal, null));
    }

    @Test
    void validateDateToTrue() {
        Weekday weekday = Weekday.any(DayOfWeek.values());
        assertTrue(weekday.validate(new Date(), null));
    }

    @Test
    void validateDateToFalse() {
        Weekday weekday = Weekday.any(MONDAY, THURSDAY, WEDNESDAY, THURSDAY, FRIDAY, SUNDAY);
        Date date = Date.from(LocalDate.of(2022, 1, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
        assertFalse(weekday.validate(date, null));
    }

    @Test
    void serializeWeekdaysAny() {
        Weekday any = Weekday.any(DayOfWeek.values());
        assertEquals("""
                "type":"WEEKDAY_ANY","values":["MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY"]""",
                any.serializeToJson());
    }

    @Test
    void serializeWeekdaysAnyOrNull() {
        Weekday anyOrNull = Weekday.anyOrNull(MONDAY, SUNDAY);
        assertEquals("""
                "type":"WEEKDAY_ANY","values":["MONDAY","SUNDAY"],"nullEqualsTo":true""", anyOrNull.serializeToJson());
    }

}

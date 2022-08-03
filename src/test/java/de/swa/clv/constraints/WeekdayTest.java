package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.Test;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Calendar;
import java.util.Date;

import static java.time.DayOfWeek.*;
import static java.time.DayOfWeek.MONDAY;
import static org.junit.Assert.*;

public class WeekdayTest {

    @Test
    public void anyValuesMustNotBeNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                Weekday::any);
        assertEquals("Provide at least one value", ex.getMessage());
    }

    @Test
    public void anyValuesMustNotContainNulls() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Weekday.any(SUNDAY, null));
        assertEquals("Null values are not allowed", ex.getMessage());
    }

    @Test
    public void anyOrNullValuesMustNotContainNulls() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Weekday.anyOrNull(SUNDAY, null));
        assertEquals("Null values are not allowed", ex.getMessage());
    }

    @Test
    public void unsupportedType() {
        Dates weekday = Weekday.any(SUNDAY);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> weekday.validate("string is unsupported", null));
        assertEquals("Unsupported type: class java.lang.String", ex.getMessage());
    }

    @Test
    public void validateNullObjectToFalse() {
        Weekday weekday = Weekday.any(SUNDAY);
        assertFalse(weekday.validate(null, null));
    }

    @Test
    public void validateNullObjectToTrue() {
        Weekday weekday = Weekday.anyOrNull(SUNDAY);
        assertTrue(weekday.validate(null, null));
    }

    @Test
    public void validateLocalDateToTrue() {
        Dates weekday = Weekday.any(SATURDAY);
        assertTrue(weekday.validate(LocalDate.of(2022, 1, 1), null));
    }

    @Test
    public void validateLocalDateToFalse() {
        Dates weekday = Weekday.any(MONDAY, THURSDAY, WEDNESDAY, THURSDAY, FRIDAY, SUNDAY);
        assertFalse(weekday.validate(LocalDate.of(2022, 1, 1), null));
    }

    @Test
    public void validateLocalDateTimeToTrue() {
        Dates weekday = Weekday.any(SATURDAY);
        assertTrue(weekday.validate(LocalDateTime.of(2022, 1, 1, 20, 15), null));
    }

    @Test
    public void validateLocalDateTimeToFalse() {
        Dates weekday = Weekday.any(MONDAY, THURSDAY, WEDNESDAY, THURSDAY, FRIDAY, SUNDAY);
        assertFalse(weekday.validate(LocalDateTime.of(2022, 1, 1, 20, 15), null));
    }

    @Test
    public void validateCalendarToTrue() {
        Dates weekday = Weekday.any(DayOfWeek.values());
        assertTrue(weekday.validate(Calendar.getInstance(), null));
    }

    @Test
    public void validateCalendarToFalse() {
        Dates weekday = Weekday.any(MONDAY, THURSDAY, WEDNESDAY, THURSDAY, FRIDAY, SUNDAY);
        Calendar cal = Calendar.getInstance();
        cal.set(2022, Calendar.JANUARY, 1);
        assertFalse(weekday.validate(cal, null));
    }

    @Test
    public void validateDateToTrue() {
        Dates weekday = Weekday.any(DayOfWeek.values());
        assertTrue(weekday.validate(new Date(), null));
    }

    @Test
    public void validateDateToFalse() {
        Dates weekday = Weekday.any(MONDAY, THURSDAY, WEDNESDAY, THURSDAY, FRIDAY, SUNDAY);
        Calendar cal = Calendar.getInstance();
        cal.set(2022, Calendar.JANUARY, 1);
        Date date = new Date();
        date.setTime(cal.getTimeInMillis());
        assertFalse(weekday.validate(date, null));
    }

    @Test
    public void serializeWeekdaysAny() {
        Dates weekday = Weekday.any(DayOfWeek.values());
        assertEquals("""
                "type":"WEEKDAY_ANY","days":["MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY"]""",
                weekday.serializeToJson());
    }

    @Test
    public void serializeWeekdaysAnyOrNull() {
        Weekday weekday = Weekday.anyOrNull(MONDAY, SUNDAY);
        assertEquals("""
                "type":"WEEKDAY_ANY","days":["MONDAY","SUNDAY","null"]""", weekday.serializeToJson());
    }

}

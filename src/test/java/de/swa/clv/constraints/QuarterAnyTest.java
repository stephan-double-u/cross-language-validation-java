package de.swa.clv.constraints;

import org.junit.jupiter.api.Test;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.TemporalField;
import java.util.Calendar;
import java.util.Date;

import static de.swa.clv.constraints.Constraint.EMPTY_VALUES_ERR_MESSAGE;
import static de.swa.clv.constraints.Constraint.NULL_VALUE_ERR_MESSAGE;
import static de.swa.clv.constraints.Quarter.QUARTERS_ERR_MESSAGE;
import static java.time.DayOfWeek.*;
import static org.junit.jupiter.api.Assertions.*;

public class QuarterAnyTest {

    @Test
    public void anyValuesMustNotBeNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                Quarter::any);
        assertEquals(EMPTY_VALUES_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    public void anyValuesMustNotContainNulls() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Quarter.any(1, null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    public void anyOrNullValuesMustNotContainNulls() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Quarter.anyOrNull(1, null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    public void valuesMustBeValidQuarters() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Quarter.any(0, 5));
        assertEquals(QUARTERS_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    public void unsupportedType() {
        QuarterAny any = Quarter.any(1);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> any.validate("string is unsupported", null));
        assertEquals("Unsupported type: class java.lang.String", ex.getMessage());
    }

    @Test
    public void validateNullObjectToFalse() {
        QuarterAny any = Quarter.any(1);
        assertFalse(any.validate(null, null));
    }

    @Test
    public void validateNullObjectToTrue() {
        QuarterAny any = Quarter.anyOrNull(1);
        assertTrue(any.validate(null, null));
    }

    @Test
    public void validateLocalDateToTrue() {
        QuarterAny any = Quarter.any(1);
        assertTrue(any.validate(LocalDate.of(2022, 1, 1), null));
    }

    @Test
    public void validateLocalDateToFalse() {
        QuarterAny any = Quarter.any(2, 3, 4);
        assertFalse(any.validate(LocalDate.of(2022, 1, 1), null));
    }

    @Test
    public void validateLocalDateTimeToTrue() {
        QuarterAny any = Quarter.any(1);
        assertTrue(any.validate(LocalDateTime.of(2022, 1, 1, 20, 15), null));
    }

    @Test
    public void validateLocalDateTimeToFalse() {
        QuarterAny any = Quarter.any(2);
        assertFalse(any.validate(LocalDateTime.of(2022, 1, 1, 20, 15), null));
    }

    @Test
    public void validateCalendarToTrue() {
        QuarterAny any = Quarter.any(1);
        Calendar cal = Calendar.getInstance();
        cal.set(2022, Calendar.JANUARY, 1);
        assertTrue(any.validate(cal, null));
    }

    @Test
    public void validateCalendarToFalse() {
        QuarterAny any = Quarter.any(2);
        Calendar cal = Calendar.getInstance();
        cal.set(2022, Calendar.JANUARY, 1);
        assertFalse(any.validate(cal, null));
    }

    @Test
    public void validateUtilDateToTrue() {
        QuarterAny any = Quarter.any(1);
        Date date = Date.from(LocalDate.of(2022, 1, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
        assertTrue(any.validate(date, null));
    }

    @Test
    public void validateUtilDateToFalse() {
        QuarterAny any = Quarter.any(2);
        Date date = Date.from(LocalDate.of(2022, 1, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
        assertFalse(any.validate(date, null));
    }

    @Test
    public void serializeQuartersAny() {
        QuarterAny any = Quarter.any(1, 3);
        assertEquals("""
                "type":"QUARTER_ANY","values":[1,3]""",
                any.serializeToJson());
    }

    @Test
    public void serializeQuartersAnyOrNull() {
        QuarterAny any = Quarter.anyOrNull(4, 2);
        assertEquals("""
                "type":"QUARTER_ANY","values":[4,2],"nullEqualsTo":true""",
                any.serializeToJson());
    }

}

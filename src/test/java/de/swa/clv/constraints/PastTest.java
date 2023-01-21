package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Calendar;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

class PastTest {

    @Test
    void pastDaysMinGreaterZero() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Past.minDays(-1));
        assertEquals("'minDays' and 'maxDays' values must not be < 0", ex.getMessage());
    }

    @Test
    void pastDaysMaxGreaterZero() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Past.minMaxDays(0, -1));
        assertEquals("'minDays' and 'maxDays' values must not be < 0", ex.getMessage());
    }

    @Test
    void pastDaysMinNotGreaterMax() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Past.minMaxDays(1, 0));
        assertEquals("'minDays' value must not be greater than 'maxDays' value", ex.getMessage());
    }

    @Test
    void unsupportedType() {
        Dates dates = Past.minDays(0);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> dates.validate("string is unsupported", null));
        assertEquals("Unsupported type: class java.lang.String", ex.getMessage());
    }

    @Test
    void validateNullObjectToFalse() {
        Dates dates = Past.minDays(0);
        assertFalse(dates.validate(null, null));
    }

    void validateNullObjectToTrue() {
        Dates dates = Past.minDaysOrNull(0);
        assertTrue(dates.validate(null, null));
    }

    @Test
    void validateLocalDateToNowIsTrue() {
        Dates dates = Past.minMaxDays(0, 0);
        assertTrue(dates.validate(LocalDate.now(), null));
    }

    @Test
    void validateLocalDateIsTrue() {
        Dates dates = Past.minMaxDays(1, 1);
        assertTrue(dates.validate(LocalDate.now().minusDays(1), null));
    }

    @Test
    void validateLocalDateMinDaysToFalse() {
        Dates dates = Past.minDays(3);
        assertFalse(dates.validate(LocalDate.now().minusDays(2), null));
    }

    @Test
    void validateLocalDateMaxDaysToFalse() {
        Dates dates = Past.minMaxDays(1, 3);
        assertFalse(dates.validate(LocalDate.now().minusDays(4), null));
    }

    @Test
    void validateLocalDateTimeToTrue() {
        Dates dates = Past.minDays(1);
        assertTrue(dates.validate(LocalDateTime.now().minusDays(1).minusMinutes(1), null));
    }

    @Test
    void validateLocalDateTimeMinToFalse() {
        Dates dates = Past.minDays(1);
        assertFalse(dates.validate(LocalDateTime.now().minusDays(1).plusMinutes(1), null));
    }

    @Test
    void validateLocalDateTimeMaxToFalse() {
        Dates dates = Past.minMaxDays(1, 3);
        assertFalse(dates.validate(LocalDateTime.now().minusDays(3).minusMinutes(1), null));
    }

    @Test
    void validateCalendarToTrue() {
        Dates dates = Past.minDays(1000);
        Calendar pastCalDate = Calendar.getInstance();
        pastCalDate.set(2000, Calendar.DECEMBER, 31);
        assertTrue(dates.validate(pastCalDate, null));
    }

    @Test
    void validateCalendarMinToFalse() {
        Dates dates = Past.minMaxDays(2, 3);
        Calendar pastCalDate = Calendar.getInstance();
        pastCalDate.add(Calendar.DATE, -1);
        assertFalse(dates.validate(pastCalDate, null));
    }

    @Test
    void validateCalendarMaxToFalse() {
        Dates dates = Past.minMaxDays(2, 3);
        Calendar pastCalDate = Calendar.getInstance();
        pastCalDate.add(Calendar.DATE, -4);
        assertFalse(dates.validate(pastCalDate, null));
    }

    @Test
    void validateDateToTrue() {
        Dates dates = Past.minDays(1);
        final Date yesterdayMinus1Sec = new Date(new Date().getTime() - Dates.MILLIS_PER_DAY - 1000);
        assertTrue(dates.validate(yesterdayMinus1Sec, null));
    }

    @Test
    void validateDateMinToFalse() {
        Dates dates = Past.minDays(0);
        final Date nowPlus1Sec = new Date(new Date().getTime() + 1000);
        assertFalse(dates.validate(nowPlus1Sec, null));
    }

    @Test
    void validateDateMaxToFalse() {
        Dates dates = Past.minMaxDays(1, 3);
        final Date nowMinus4Days = new Date(new Date().getTime() - 4 * Dates.MILLIS_PER_DAY);
        assertFalse(dates.validate(nowMinus4Days, null));
    }

    @Test
    void serializePastDaysMin() {
        Dates past = Past.minDays(1);
        assertEquals("""
                "type":"PAST_DAYS","min":1""", past.serializeToJson());
    }

    @Test
    void serializePastDaysMinOrNull() {
        Dates past = Past.minDaysOrNull(1);
        assertEquals("""
                "type":"PAST_DAYS","min":1,"nullEqualsTo":true""", past.serializeToJson());
    }

    @Test
    void serializePastDaysMinMax() {
        Dates past = Past.minMaxDays(1, 2);
        assertEquals("""
                "type":"PAST_DAYS","min":1,"max":2""", past.serializeToJson());
    }

    @Test
    void serializePastDaysMinMaxOrNull() {
        Dates past = Past.minMaxDaysOrNull(1, 2);
        assertEquals("""
                "type":"PAST_DAYS","min":1,"max":2,"nullEqualsTo":true""", past.serializeToJson());
    }

}

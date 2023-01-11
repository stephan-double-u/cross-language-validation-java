package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Calendar;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

public class PastTest {

    @Test
    public void pastDaysMinGreaterZero() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Past.minDays(-1));
        assertEquals("'minDays' and 'maxDays' values must not be < 0", ex.getMessage());
    }

    @Test
    public void pastDaysMaxGreaterZero() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Past.minMaxDays(0, -1));
        assertEquals("'minDays' and 'maxDays' values must not be < 0", ex.getMessage());
    }

    @Test
    public void pastDaysMinNotGreaterMax() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Past.minMaxDays(1, 0));
        assertEquals("'minDays' value must not be greater than 'maxDays' value", ex.getMessage());
    }

    @Test
    public void unsupportedType() {
        Dates dates = Past.minDays(0);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> dates.validate("string is unsupported", null));
        assertEquals("Unsupported type: class java.lang.String", ex.getMessage());
    }

    @Test
    public void validateNullObjectToFalse() {
        Dates dates = Past.minDays(0);
        assertFalse(dates.validate(null, null));
    }

    @Test
    public void validateLocalDateToNowIsTrue() {
        Dates dates = Past.minMaxDays(0, 0);
        assertTrue(dates.validate(LocalDate.now(), null));
    }

    @Test
    public void validateLocalDateIsTrue() {
        Dates dates = Past.minMaxDays(1, 1);
        assertTrue(dates.validate(LocalDate.now().minusDays(1), null));
    }

    @Test
    public void validateLocalDateMinDaysToFalse() {
        Dates dates = Past.minDays(3);
        assertFalse(dates.validate(LocalDate.now().minusDays(2), null));
    }

    @Test
    public void validateLocalDateMaxDaysToFalse() {
        Dates dates = Past.minMaxDays(1, 3);
        assertFalse(dates.validate(LocalDate.now().minusDays(4), null));
    }

    @Test
    public void validateLocalDateTimeToTrue() {
        Dates dates = Past.minDays(1);
        assertTrue(dates.validate(LocalDateTime.now().minusDays(1).minusMinutes(1), null));
    }

    @Test
    public void validateLocalDateTimeMinToFalse() {
        Dates dates = Past.minDays(1);
        assertFalse(dates.validate(LocalDateTime.now().minusDays(1).plusMinutes(1), null));
    }

    @Test
    public void validateLocalDateTimeMaxToFalse() {
        Dates dates = Past.minMaxDays(1, 3);
        assertFalse(dates.validate(LocalDateTime.now().minusDays(3).minusMinutes(1), null));
    }

    @Test
    public void validateCalendarToTrue() {
        Dates dates = Past.minDays(1000);
        Calendar pastCalDate = Calendar.getInstance();
        pastCalDate.set(2000, Calendar.DECEMBER, 31);
        assertTrue(dates.validate(pastCalDate, null));
    }

    @Test
    public void validateCalendarMinToFalse() {
        Dates dates = Past.minMaxDays(2, 3);
        Calendar pastCalDate = Calendar.getInstance();
        pastCalDate.add(Calendar.DATE, -1);
        assertFalse(dates.validate(pastCalDate, null));
    }

    @Test
    public void validateCalendarMaxToFalse() {
        Dates dates = Past.minMaxDays(2, 3);
        Calendar pastCalDate = Calendar.getInstance();
        pastCalDate.add(Calendar.DATE, -4);
        assertFalse(dates.validate(pastCalDate, null));
    }

    @Test
    public void validateDateToTrue() {
        Dates dates = Past.minDays(1);
        final Date yesterdayMinus1Sec = new Date(new Date().getTime() - Dates.MILLIS_PER_DAY - 1000);
        assertTrue(dates.validate(yesterdayMinus1Sec, null));
    }

    @Test
    public void validateDateMinToFalse() {
        Dates dates = Past.minDays(0);
        final Date nowPlus1Sec = new Date(new Date().getTime() + 1000);
        assertFalse(dates.validate(nowPlus1Sec, null));
    }

    @Test
    public void validateDateMaxToFalse() {
        Dates dates = Past.minMaxDays(1, 3);
        final Date nowMinus4Days = new Date(new Date().getTime() - 4 * Dates.MILLIS_PER_DAY);
        assertFalse(dates.validate(nowMinus4Days, null));
    }

    @Test
    public void serializePastDaysMin() {
        Dates dates = Past.minDays(1);
        assertEquals(Util.doubleQuote("'type':'PAST_DAYS','min':1"), dates.serializeToJson());
    }

    @Test
    public void serializePastDaysMinMax() {
        Dates dates = Past.minMaxDays(1, 2);
        assertEquals(Util.doubleQuote("'type':'PAST_DAYS','min':1,'max':2"), dates.serializeToJson());
    }

}

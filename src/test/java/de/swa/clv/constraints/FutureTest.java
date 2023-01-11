package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Calendar;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

public class FutureTest {

    @Test
    public void futureDaysMinGreaterZero() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Future.minDays(-1));
        assertEquals("'minDays' and 'maxDays' values must not be < 0", ex.getMessage());
    }

    @Test
    public void futureDaysMaxGreaterZero() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Future.minMaxDays(0, -1));
        assertEquals("'minDays' and 'maxDays' values must not be < 0", ex.getMessage());
    }

    @Test
    public void futureDaysMinNotGreaterMax() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Future.minMaxDays(1, 0));
        assertEquals("'minDays' value must not be greater than 'maxDays' value", ex.getMessage());
    }

    @Test
    public void unsupportedType() {
        Dates dates = Future.minDays(0);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> dates.validate("string is unsupported", null));
        assertEquals("Unsupported type: class java.lang.String", ex.getMessage());
    }

    @Test
    public void validateNullObjectToFalse() {
        Dates dates = Future.minDays(0);
        assertFalse(dates.validate(null, null));
    }

    @Test
    public void validateLocalDateToNowIsTrue() {
        Dates dates = Future.minMaxDays(0, 0);
        assertTrue(dates.validate(LocalDate.now(), null));
    }

    @Test
    public void validateLocalDateIsTrue() {
        Dates dates = Future.minMaxDays(1, 1);
        assertTrue(dates.validate(LocalDate.now().plusDays(1), null));
    }

    @Test
    public void validateLocalDateMinDaysToFalse() {
        Dates dates = Future.minDays(3);
        assertFalse(dates.validate(LocalDate.now().plusDays(2), null));
    }

    @Test
    public void validateLocalDateMaxDaysToFalse() {
        Dates dates = Future.minMaxDays(1, 3);
        assertFalse(dates.validate(LocalDate.now().plusDays(4), null));
    }

    @Test
    public void validateLocalDateTimeToTrue() {
        Dates dates = Future.minDays(1);
        assertTrue(dates.validate(LocalDateTime.now().plusDays(1).plusMinutes(1), null));
    }

    @Test
    public void validateLocalDateTimeMinToFalse() {
        Dates dates = Future.minDays(1);
        assertFalse(dates.validate(LocalDateTime.now().plusDays(1).minusMinutes(1), null));
    }

    @Test
    public void validateLocalDateTimeMaxToFalse() {
        Dates dates = Future.minMaxDays(1, 3);
        assertFalse(dates.validate(LocalDateTime.now().plusDays(3).plusMinutes(1), null));
    }

    @Test
    public void validateCalendarToTrue() {
        Dates dates = Future.minDays(1000);
        Calendar calDate = Calendar.getInstance();
        calDate.set(2999, Calendar.DECEMBER, 31);
        assertTrue(dates.validate(calDate, null));
    }

    @Test
    public void validateCalendarMinToFalse() {
        Dates dates = Future.minMaxDays(2, 3);
        Calendar calDate = Calendar.getInstance();
        calDate.add(Calendar.DATE, 1);
        assertFalse(dates.validate(calDate, null));
    }

    @Test
    public void validateCalendarMaxToFalse() {
        Dates dates = Future.minMaxDays(2, 3);
        Calendar calDate = Calendar.getInstance();
        calDate.add(Calendar.DATE, 4);
        assertFalse(dates.validate(calDate, null));
    }

    @Test
    public void validateDateToTrue() {
        Dates dates = Future.minDays(1);
        final Date tomorrowPlus1Sec = new Date(new Date().getTime() + Dates.MILLIS_PER_DAY + 1000);
        assertTrue(dates.validate(tomorrowPlus1Sec, null));
    }

    @Test
    public void validateDateMinToFalse() {
        //Dates asdasd = Weekday.any(MON, THU, FRI);
        DayOfWeek dayOfWeek = LocalDate.now().getDayOfWeek();

        Dates dates = Future.minDays(0);
        final Date nowMinus1Sec = new Date(new Date().getTime() - 1000);
        assertFalse(dates.validate(nowMinus1Sec, null));
    }

    @Test
    public void validateDateMaxToFalse() {
        Dates dates = Future.minMaxDays(1, 3);
        final Date nowPlus4Days = new Date(new Date().getTime() + 4 * Dates.MILLIS_PER_DAY);
        assertFalse(dates.validate(nowPlus4Days, null));
    }

    @Test
    public void serializeFutureDaysMin() {
        Dates dates = Future.minDays(1);
        assertEquals(Util.doubleQuote("'type':'FUTURE_DAYS','min':1"), dates.serializeToJson());
    }

    @Test
    public void serializeFutureDaysMinMax() {
        Dates dates = Future.minMaxDays(1, 2);
        assertEquals(Util.doubleQuote("'type':'FUTURE_DAYS','min':1,'max':2"), dates.serializeToJson());
    }

}

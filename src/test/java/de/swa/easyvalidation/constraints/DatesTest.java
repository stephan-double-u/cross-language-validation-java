package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.test.Util;
import org.junit.Test;

import java.time.LocalDate;
import java.util.Calendar;
import java.util.Date;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class DatesTest {

    @Test
    public void validateNullObjectToTrue() {
        Dates dates = Dates.future();
        assertTrue(dates.validate(null, null));
    }

    @Test
    public void validateFutureLocalDateToNowIsFalse() {
        Dates dates = Dates.future();
        assertFalse(dates.validate(LocalDate.now(), null));
    }

    @Test
    public void validatePastLocalDateToNowIsFalse() {
        Dates dates = Dates.past();
        assertFalse(dates.validate(LocalDate.now(), null));
    }

    @Test
    public void validateFutureLocalDateIsTrue() {
        Dates dates = Dates.future(1);
        assertTrue(dates.validate(LocalDate.now().plusDays(2), null));
    }

    @Test
    public void validatePastLocalDateIsTrue() {
        Dates dates = Dates.past(1);
        assertTrue(dates.validate(LocalDate.now().minusDays(2), null));
    }

    @Test
    public void validateFutureLocalDateToFalse() {
        Dates dates = Dates.future(3);
        assertFalse(dates.validate(LocalDate.now().plusDays(2), null));
    }

    @Test
    public void validatePastLocalDateToFalse() {
        Dates dates = Dates.past(3);
        assertFalse(dates.validate(LocalDate.now().minusDays(2), null));
    }

    @Test
    public void validateCalendarToTrue() {
        Dates dates = Dates.future(1);
        Calendar futureCalDate = Calendar.getInstance();
        futureCalDate.set(2999, 12, 31);
        assertTrue(dates.validate(futureCalDate, null));
    }

    @Test
    public void validateDateToTrue() {
        Dates dates = Dates.future(1);
        final Date tomorrowPlus1Sec = new Date(new Date().getTime() + 24 * 60 * 60 * 1000 + 1000);
        assertTrue(dates.validate(tomorrowPlus1Sec, null));
    }

    @Test
    public void serializeDateFuture() {
        Dates dates = Dates.future(1);
        assertEquals(Util.doubleQuote("'type':'DATE_FUTURE','days':1"), dates.serializeToJson());
    }

    @Test
    public void serializeDatePast() {
        Dates dates = Dates.past(2);
        assertEquals(Util.doubleQuote("'type':'DATE_PAST','days':2"), dates.serializeToJson());
    }
}

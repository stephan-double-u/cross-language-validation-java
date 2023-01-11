package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Calendar;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

public class PeriodTest {

    @Test
    public void periodDaysMinNotGreaterThanMax() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Period.minMaxDays(-10, -11));
        assertEquals("'minDays' value must not be greater than 'maxDays' value", ex.getMessage());
    }

    @Test
    public void validateNullObjectToFalse() {
        Dates dates = Period.minDays(0);
        assertFalse(dates.validate(null, null));
    }

    @Test
    public void validateLocalDateToNowIsTrue() {
        Dates dates = Period.minMaxDays(0, 0);
        assertTrue(dates.validate(LocalDate.now(), null));
    }

    @Test
    public void validateLocalDateToTrue() {
        Dates dates = Period.minMaxDays(-10, -8);
        assertTrue(dates.validate(LocalDate.now().minusDays(9), null));
    }

    @Test
    public void validateLocalDateMinDaysToFalse() {
        Dates dates = Period.minMaxDays(-10, -8);
        assertFalse(dates.validate(LocalDate.now().minusDays(11), null));
    }

    @Test
    public void validateLocalDateMaxDaysToFalse() {
        Dates dates = Period.minMaxDays(-10, 10);
        assertFalse(dates.validate(LocalDate.now().plusDays(11), null));
    }

    @Test
    public void validateLocalDateTimeToTrue() {
        Dates dates = Period.minDays(-1);
        assertTrue(dates.validate(LocalDateTime.now().minusDays(1).plusDays(1), null));
    }

    @Test
    public void validateLocalDateTimeMinDaysToFalse() {
        Dates dates = Period.minDays(-1);
        assertFalse(dates.validate(LocalDateTime.now().minusDays(2), null));
    }

    @Test
    public void validateLocalDateTimeMaxDaysToFalse() {
        Dates dates = Period.minMaxDays(-1, 1);
        assertFalse(dates.validate(LocalDateTime.now().plusDays(2), null));
    }

    @Test
    public void validateCalendarToTrue() {
        Dates dates = Period.maxDays(1);
        assertTrue(dates.validate(Calendar.getInstance(), null));
    }

    @Test
    public void validateCalendarMinDaysToFalse() {
        Dates dates = Period.minDays(-1);
        Calendar calDate = Calendar.getInstance();
        calDate.add(Calendar.DATE, -2);
        assertFalse(dates.validate(calDate, null));
    }

    @Test
    public void validateCalendarMaxDaysToFalse() {
        Dates dates = Period.maxDays(1);
        Calendar calDate = Calendar.getInstance();
        calDate.add(Calendar.DATE, 2);
        assertFalse(dates.validate(calDate, null));
    }

    @Test
    public void validateDateToTrue() {
        Dates dates = Period.minMaxDays(0, 0);
        assertTrue(dates.validate(new Date(), null));
    }

    @Test
    public void validateDateMinDaysToFalse() {
        Dates dates = Period.minDays(-1);
        final Date nowMinus2Days = new Date(new Date().getTime() - 2 * Dates.MILLIS_PER_DAY);
        assertFalse(dates.validate(nowMinus2Days, null));
    }

    @Test
    public void validateDateMaxDaysToFalse() {
        Dates dates = Period.maxDays(1);
        final Date nowPlus2Days = new Date(new Date().getTime() + 2 * Dates.MILLIS_PER_DAY);
        assertFalse(dates.validate(nowPlus2Days, null));
    }

    @Test
    public void serializePeriodDaysMin() {
        Dates dates = Period.minDays(-10);
        assertEquals("""
                "type":"PERIOD_DAYS","min":-10""", dates.serializeToJson());
    }

    @Test
    public void serializePeriodDaysMax() {
        Dates dates = Period.maxDays(10);
        assertEquals("""
                "type":"PERIOD_DAYS","max":10""", dates.serializeToJson());
    }

    @Test
    public void serializePeriodDaysMinMax() {
        Dates dates = Period.minMaxDays(-10, 10);
        assertEquals("""
                "type":"PERIOD_DAYS","min":-10,"max":10""", dates.serializeToJson());
    }
}

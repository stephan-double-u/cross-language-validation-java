package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Calendar;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

class PeriodTest {

    @Test
    void periodDaysMinNotGreaterThanMax() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Period.minMaxDays(-10, -11));
        assertEquals("'minDays' value must not be greater than 'maxDays' value", ex.getMessage());
    }

    @Test
    void validateNullObjectToFalse() {
        Dates period = Period.minDays(0);
        assertFalse(period.validate(null, null));
    }

    @Test
    void validateNullObjectToTrue() {
        Dates period = Period.minDaysOrNull(0);
        assertTrue(period.validate(null, null));
    }

    @Test
    void validateLocalDateToNowIsTrue() {
        Dates period = Period.minMaxDays(0, 0);
        assertTrue(period.validate(LocalDate.now(), null));
    }

    @Test
    void validateLocalDateToTrue() {
        Dates period = Period.minMaxDays(-10, -8);
        assertTrue(period.validate(LocalDate.now().minusDays(9), null));
    }

    @Test
    void validateLocalDateMinDaysToFalse() {
        Dates period = Period.minMaxDays(-10, -8);
        assertFalse(period.validate(LocalDate.now().minusDays(11), null));
    }

    @Test
    void validateLocalDateMaxDaysToFalse() {
        Dates period = Period.minMaxDays(-10, 10);
        assertFalse(period.validate(LocalDate.now().plusDays(11), null));
    }

    @Test
    void validateLocalDateTimeToTrue() {
        Dates period = Period.minDays(-1);
        assertTrue(period.validate(LocalDateTime.now().minusDays(1).plusDays(1), null));
    }

    @Test
    void validateLocalDateTimeMinDaysToFalse() {
        Dates period = Period.minDays(-1);
        assertFalse(period.validate(LocalDateTime.now().minusDays(2), null));
    }

    @Test
    void validateLocalDateTimeMaxDaysToFalse() {
        Dates period = Period.minMaxDays(-1, 1);
        assertFalse(period.validate(LocalDateTime.now().plusDays(2), null));
    }

    @Test
    void validateCalendarToTrue() {
        Dates period = Period.maxDays(1);
        assertTrue(period.validate(Calendar.getInstance(), null));
    }

    @Test
    void validateCalendarMinDaysToFalse() {
        Dates period = Period.minDays(-1);
        Calendar calDate = Calendar.getInstance();
        calDate.add(Calendar.DATE, -2);
        assertFalse(period.validate(calDate, null));
    }

    @Test
    void validateCalendarMaxDaysToFalse() {
        Dates period = Period.maxDays(1);
        Calendar calDate = Calendar.getInstance();
        calDate.add(Calendar.DATE, 2);
        assertFalse(period.validate(calDate, null));
    }

    @Test
    void validateDateToTrue() {
        Dates period = Period.minMaxDays(0, 0);
        assertTrue(period.validate(new Date(), null));
    }

    @Test
    void validateDateMinDaysToFalse() {
        Dates period = Period.minDays(-1);
        final Date nowMinus2Days = new Date(new Date().getTime() - 2 * Dates.MILLIS_PER_DAY);
        assertFalse(period.validate(nowMinus2Days, null));
    }

    @Test
    void validateDateMaxDaysToFalse() {
        Dates period = Period.maxDays(1);
        final Date nowPlus2Days = new Date(new Date().getTime() + 2 * Dates.MILLIS_PER_DAY);
        assertFalse(period.validate(nowPlus2Days, null));
    }

    @Test
    void serializePeriodDaysMin() {
        Dates period = Period.minDays(-10);
        assertEquals("""
                "type":"PERIOD_DAYS","min":-10""", period.serializeToJson());
    }

    @Test
    void serializePeriodDaysMinOrNull() {
        Dates period = Period.minDaysOrNull(-10);
        assertEquals("""
                "type":"PERIOD_DAYS","min":-10,"nullEqualsTo":true""", period.serializeToJson());
    }

    @Test
    void serializePeriodDaysMax() {
        Dates period = Period.maxDays(10);
        assertEquals("""
                "type":"PERIOD_DAYS","max":10""", period.serializeToJson());
    }

    @Test
    void serializePeriodDaysMaxOrNull() {
        Dates period = Period.maxDaysOrNull(10);
        assertEquals("""
                "type":"PERIOD_DAYS","max":10,"nullEqualsTo":true""", period.serializeToJson());
    }

    @Test
    void serializePeriodDaysMinMax() {
        Dates period = Period.minMaxDays(-10, 10);
        assertEquals("""
                "type":"PERIOD_DAYS","min":-10,"max":10""", period.serializeToJson());
    }

    @Test
    void serializePeriodDaysMinMaxOrNull() {
        Dates period = Period.minMaxDaysOrNull(-10, 10);
        assertEquals("""
                "type":"PERIOD_DAYS","min":-10,"max":10,"nullEqualsTo":true""", period.serializeToJson());
    }
}

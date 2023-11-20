package de.swa.clv.constraints;

import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

class YearRangeTest {

    @Test
    void validateMinMaxRelativeToTrue() {
        YearRange range = Year.minMaxRelative(-1, 0);
        assertTrue(range.validate(LocalDate.now(), null));
    }

    @Test
    void validateMinMaxRelativeToFalse() {
        YearRange range = Year.minMaxRelative(1, 2);
        assertFalse(range.validate(LocalDate.now(), null));
    }

    @Test
    void validateMinMaxRelativeToFalse4Null() {
        YearRange range = Year.minMaxRelative(-1, 2);
        assertFalse(range.validate(null, null));
    }

    @Test
    void validateMinMaxRelativeOrNullToTrue() {
        YearRange range = Year.minMaxRelativeOrNull(-1, 2);
        assertTrue(range.validate(LocalDate.now(), null));
    }

    @Test
    void validateMinMaxRelativeOrNullToFalse() {
        YearRange range = Year.minMaxRelativeOrNull(1, 2);
        assertFalse(range.validate(LocalDate.now(), null));
    }

    @Test
    void validateMinMaxRelativeOrNullToTrue4Null() {
        YearRange range = Year.minMaxRelativeOrNull(-1, 2);
        assertTrue(range.validate(null, null));
    }


    @Test
    void validateMinRelativeToTrue() {
        YearRange range = Year.minRelative(-1);
        assertTrue(range.validate(LocalDate.now(), null));
    }

    @Test
    void validateMinRelativeToFalse() {
        YearRange range = Year.minRelative(1);
        assertFalse(range.validate(LocalDate.now(), null));
    }

    @Test
    void validateMinRelativeToFalse4Null() {
        YearRange range = Year.minRelative(-1);
        assertFalse(range.validate(null, null));
    }

    @Test
    void validateMinRelativeOrNullToTrue() {
        YearRange range = Year.minRelativeOrNull(-1);
        assertTrue(range.validate(LocalDate.now(), null));
    }

    @Test
    void validateMinRelativeOrNullToFalse() {
        YearRange range = Year.minRelativeOrNull(1);
        assertFalse(range.validate(LocalDate.now(), null));
    }

    @Test
    void validateMinRelativeOrNullToTrue4Null() {
        YearRange range = Year.minRelativeOrNull(-1);
        assertTrue(range.validate(null, null));
    }


    @Test
    void validateMaxRelativeToTrue() {
        YearRange range = Year.maxRelative(1);
        assertTrue(range.validate(LocalDate.now(), null));
    }

    @Test
    void validateMaxRelativeToFalse() {
        YearRange range = Year.maxRelative(-1);
        assertFalse(range.validate(LocalDate.now(), null));
    }

    @Test
    void validateMaxRelativeToFalse4Null() {
        YearRange range = Year.maxRelative(1);
        assertFalse(range.validate(null, null));
    }

    @Test
    void validateMaxRelativeOrNullToTrue() {
        YearRange range = Year.maxRelativeOrNull(1);
        assertTrue(range.validate(LocalDate.now(), null));
    }

    @Test
    void validateMaxRelativeOrNullToFalse() {
        YearRange range = Year.maxRelativeOrNull(-1);
        assertFalse(range.validate(LocalDate.now(), null));
    }

    @Test
    void validateMaxRelativeOrNullToTrue4Null() {
        YearRange range = Year.maxRelativeOrNull(1);
        assertTrue(range.validate(null, null));
    }


    @Test
    void validateMinMaxAbsoluteToTrue() {
        YearRange range = Year.minMaxAbsolute(2020, 2021);
        assertTrue(range.validate(LocalDate.of(2020, 1, 31), null));
    }

    @Test
    void validateMinMaxAbsoluteToFalse() {
        YearRange range = Year.minMaxAbsolute(2020, 2021);
        assertFalse(range.validate(LocalDate.of(2019, 1, 31), null));
    }

    @Test
    void validateMinMaxAbsoluteToFalse4Null() {
        YearRange range = Year.minMaxAbsolute(2020, 2021);
        assertFalse(range.validate(null, null));
    }

    @Test
    void validateMinMaxAbsoluteOrNullToTrue() {
        YearRange range = Year.minMaxAbsoluteOrNull(2020, 2021);
        assertTrue(range.validate(LocalDate.of(2020, 1, 31), null));
    }

    @Test
    void validateMinMaxAbsoluteOrNullToFalse() {
        YearRange range = Year.minMaxAbsoluteOrNull(2020, 2021);
        assertFalse(range.validate(LocalDate.of(2019, 1, 31), null));
    }

    @Test
    void validateMinMaxAbsoluteOrNullToTrue4Null() {
        YearRange range = Year.minMaxAbsoluteOrNull(2020, 2021);
        assertTrue(range.validate(null, null));
    }


    @Test
    void validateMinAbsoluteToTrue() {
        YearRange range = Year.minAbsolute(2020);
        assertTrue(range.validate(LocalDate.of(2020, 1, 31), null));
    }

    @Test
    void validateMinAbsoluteToFalse() {
        YearRange range = Year.minAbsolute(2020);
        assertFalse(range.validate(LocalDate.of(2019, 1, 31), null));
    }

    @Test
    void validateMinAbsoluteToFalse4Null() {
        YearRange range = Year.minAbsolute(2020);
        assertFalse(range.validate(null, null));
    }

    @Test
    void validateMinAbsoluteOrNullToTrue() {
        YearRange range = Year.minAbsoluteOrNull(2020);
        assertTrue(range.validate(LocalDate.of(2020, 1, 31), null));
    }

    @Test
    void validateMinAbsoluteOrNullToFalse() {
        YearRange range = Year.minAbsoluteOrNull(2020);
        assertFalse(range.validate(LocalDate.of(2019, 1, 31), null));
    }

    @Test
    void validateMinAbsoluteOrNullToTrue4Null() {
        YearRange range = Year.minAbsoluteOrNull(2020);
        assertTrue(range.validate(null, null));
    }


    @Test
    void validateMaxAbsoluteToTrue() {
        YearRange range = Year.maxAbsolute(2021);
        assertTrue(range.validate(LocalDate.of(2020, 1, 31), null));
    }

    @Test
    void validateMaxAbsoluteToFalse() {
        YearRange range = Year.maxAbsolute(2018);
        assertFalse(range.validate(LocalDate.of(2019, 1, 31), null));
    }

    @Test
    void validateMaxAbsoluteToFalse4Null() {
        YearRange range = Year.maxAbsolute(2021);
        assertFalse(range.validate(null, null));
    }

    @Test
    void validateMaxAbsoluteOrNullToTrue() {
        YearRange range = Year.maxAbsoluteOrNull(2021);
        assertTrue(range.validate(LocalDate.of(2020, 1, 31), null));
    }

    @Test
    void validateMaxAbsoluteOrNullToFalse() {
        YearRange range = Year.maxAbsoluteOrNull(2018);
        assertFalse(range.validate(LocalDate.of(2019, 1, 31), null));
    }

    @Test
    void validateMaxAbsoluteOrNullToTrue4Null() {
        YearRange range = Year.maxAbsoluteOrNull(2021);
        assertTrue(range.validate(null, null));
    }


    @Test
    void serializeMinMaxRelative() {
        YearRange range = Year.minMaxRelative(-1, 2);
        assertEquals("""
                "type":"YEAR_RANGE","min":-1,"max":2,"rangeType":"RELATIVE\"""",
                range.serializeToJson());
    }

    @Test
    void serializeMinMaxRelativeOrNull() {
        YearRange range = Year.minMaxRelativeOrNull(-1, 2);
        assertEquals("""
                "type":"YEAR_RANGE","min":-1,"max":2,"nullEqualsTo":true,"rangeType":"RELATIVE\"""",
                range.serializeToJson());
    }

    @Test
    void serializeMinRelative() {
        YearRange range = Year.minRelative(-1);
        assertEquals("""
                "type":"YEAR_RANGE","min":-1,"rangeType":"RELATIVE\"""",
                range.serializeToJson());
    }

    @Test
    void serializeMinRelativeOrNull() {
        YearRange range = Year.minRelativeOrNull(-1);
        assertEquals("""
                "type":"YEAR_RANGE","min":-1,"nullEqualsTo":true,"rangeType":"RELATIVE\"""",
                range.serializeToJson());
    }

    @Test
    void serializeMaxRelative() {
        YearRange range = Year.maxRelative(2);
        assertEquals("""
                "type":"YEAR_RANGE","max":2,"rangeType":"RELATIVE\"""",
                range.serializeToJson());
    }

    @Test
    void serializeMaxRelativeOrNull() {
        YearRange range = Year.maxRelativeOrNull(2);
        assertEquals("""
                "type":"YEAR_RANGE","max":2,"nullEqualsTo":true,"rangeType":"RELATIVE\"""",
                range.serializeToJson());
    }


    @Test
    void serializeMinMaxAbsolute() {
        YearRange range = Year.minMaxAbsolute(-1, 2);
        assertEquals("""
                "type":"YEAR_RANGE","min":-1,"max":2,"rangeType":"ABSOLUTE\"""",
                range.serializeToJson());
    }

    @Test
    void serializeMinMaxAbsoluteOrNull() {
        YearRange range = Year.minMaxAbsoluteOrNull(-1, 2);
        assertEquals("""
                "type":"YEAR_RANGE","min":-1,"max":2,"nullEqualsTo":true,"rangeType":"ABSOLUTE\"""",
                range.serializeToJson());
    }

    @Test
    void serializeMinAbsolute() {
        YearRange range = Year.minAbsolute(-1);
        assertEquals("""
                "type":"YEAR_RANGE","min":-1,"rangeType":"ABSOLUTE\"""",
                range.serializeToJson());
    }

    @Test
    void serializeMinAbsoluteOrNull() {
        YearRange range = Year.minAbsoluteOrNull(-1);
        assertEquals("""
                "type":"YEAR_RANGE","min":-1,"nullEqualsTo":true,"rangeType":"ABSOLUTE\"""",
                range.serializeToJson());
    }

    @Test
    void serializeMaxAbsolute() {
        YearRange range = Year.maxAbsolute(2);
        assertEquals("""
                "type":"YEAR_RANGE","max":2,"rangeType":"ABSOLUTE\"""",
                range.serializeToJson());
    }

    @Test
    void serializeMaxAbsoluteOrNull() {
        YearRange range = Year.maxAbsoluteOrNull(2);
        assertEquals("""
                "type":"YEAR_RANGE","max":2,"nullEqualsTo":true,"rangeType":"ABSOLUTE\"""",
                range.serializeToJson());
    }

}

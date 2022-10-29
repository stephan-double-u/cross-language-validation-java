package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

import static de.swa.clv.json.JsonUtil.*;

/**
 * Constraint to express expectations for date periods.
 */
public class Weekday extends Dates {

    private static final Logger log = LoggerFactory.getLogger(Weekday.class);

    public static final String TOKEN = "WEEKDAY_ANY";

    private Weekday(final boolean nullEqualsTrue, final DayOfWeek ... days) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(days));
    }

    @Override
    public String getToken() {
        return TOKEN;
    }

    public static Weekday any(DayOfWeek ... days) {
        assertValuesAndSizeOk(days);
        return new Weekday(false, days);
    }

    public static Weekday anyOrNull(DayOfWeek ... days) {
        assertValuesAndSizeOk(days);
        return new Weekday(true, days);
    }

    @Override
    public void validateValuesOrFail(final Class<?> ignore, final Class<?> ignoreToo) {
        // validation already done in public API methods
    }

    @Override
    public boolean validate(final Object objectToValidate, final Object ignored) {
        if (objectToValidate == null) {
            log.debug("'Null object equals to {}", doesNullEqualsTrue());
            return doesNullEqualsTrue();
        }
        final boolean match;
        if (objectToValidate instanceof LocalDate date) {
            match = validate(date.getDayOfWeek().getValue());
        } else if (objectToValidate instanceof LocalDateTime date) {
            match = validate(date.getDayOfWeek().getValue());
        } else if (objectToValidate instanceof Calendar date) {
            int dayOfWeek = date.get(Calendar.DAY_OF_WEEK); // Sun=1!
            int dayOfWeekIso = (dayOfWeek + 5) % 7 + 1;
            match = validate(dayOfWeekIso);
        } else if (objectToValidate instanceof Date date) {
            Calendar cal = Calendar.getInstance();
            cal.setTime(date);
            int dayOfWeek = cal.get(Calendar.DAY_OF_WEEK); // Sun=1!
            int dayOfWeekIso = (dayOfWeek + 5) % 7 + 1;
            match = validate(dayOfWeekIso);
        } else {
            throw new IllegalArgumentException("Unsupported type: " + objectToValidate.getClass());
        }
        log.debug("Date '{}' is {} {}: {}", objectToValidate, getToken(), getValues(), match);
        return match;
    }

    private boolean validate(final Integer dateDaysOfWeek) {
        return getValues().stream()
                .map(v -> ((DayOfWeek) v).getValue())
                .filter(dayOfWeek -> Objects.equals(dayOfWeek, dateDaysOfWeek))
                .map(found -> true)
                .findAny().orElse(false);
    }

    @Override
    public String serializeToJson() {
        String nullEqualsToJson = "";
        // Serialize "nullEqualsTo" key only for non-default value 'true',
        if (doesNullEqualsTrue()) {
            nullEqualsToJson = "," + asKey("nullEqualsTo") + true;
        }
        String valuesJson = "," + asKey("days") + asArray(getValues());
        return asKey("type") + quoted(getToken()) + valuesJson + nullEqualsToJson;
    }

}

package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.*;

/**
 * Constraint to express expectations for date periods.
 */
public class Weekday extends Dates {

    private static final Logger log = LoggerFactory.getLogger(Weekday.class);

    public static final String TOKEN = "WEEKDAY_ANY";
    private static final String NULL_VALUES_MESSAGE = "Null values are not allowed";
    private static final String EMPTY_VALUES_MESSAGE = "Provide at least one value";

    private Weekday(final List<Object> values) {
        setObjectValues(values);
        validateValuesOrFail(null, null);
    }

    @Override
    public String getType() {
        return TOKEN;
    }

    public static Weekday any(DayOfWeek ... days) {
        validateNotNullAndNotEmptyOrFail(days);
        return new Weekday(Arrays.asList(days));
    }

    public static Weekday anyOrNull(DayOfWeek ... values) {
        validateNotNullAndNotEmptyOrFail(values);
        ArrayList<Object> daysWithNull = new ArrayList<>(Arrays.asList(values));
        daysWithNull.add(null);
        return new Weekday(daysWithNull);
    }

    private static void validateNotNullAndNotEmptyOrFail(DayOfWeek[] values) {
        if (values == null || Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException(NULL_VALUES_MESSAGE);
        } else if (values.length == 0) {
            throw new IllegalArgumentException(EMPTY_VALUES_MESSAGE);
        }
    }

    @Override
    public void validateValuesOrFail(final Class<?> ignore, final Class<?> ignoreToo) {
        // validation already done in public API methods
    }

    @Override
    public boolean validate(final Object dateObject, final Object ignored) {
        if (dateObject == null) {
            return getValues().contains(null);
        }
        final boolean match;
        if (dateObject instanceof LocalDate date) {
            match = validate(date.getDayOfWeek().getValue());
        } else if (dateObject instanceof LocalDateTime date) {
            match = validate(date.getDayOfWeek().getValue());
        } else if (dateObject instanceof Calendar date) {
            int dayOfWeek = date.get(Calendar.DAY_OF_WEEK); // Sun=1!
            int dayOfWeekIso = (dayOfWeek + 5) % 7 + 1;
            match = validate(dayOfWeekIso);
        } else if (dateObject instanceof Date date) {
            Calendar cal = Calendar.getInstance();
            cal.setTime(date);
            int dayOfWeek = cal.get(Calendar.DAY_OF_WEEK); // Sun=1!
            int dayOfWeekIso = (dayOfWeek + 5) % 7 + 1;
            match = validate(dayOfWeekIso);
        } else {
            throw new IllegalArgumentException("Unsupported type: " + dateObject.getClass());
        }
        log.debug("Date '{}' is {} {}: {}", dateObject, getType(), getValues(), match);
        return match;
    }

    private boolean validate(final Integer dateDaysOfWeek) {
        return getValues().stream()
                .filter(Objects::nonNull)
                .map(v -> ((DayOfWeek) v).getValue())
                .filter(dayOfWeek -> Objects.equals(dayOfWeek, dateDaysOfWeek))
                .map(found -> true)
                .findAny().orElse(false);
    }

    @Override
    public String serializeToJson() {
        String valuesJson = getValues().stream()
                .map(v -> quoted("" + v))
                .collect(Collectors.joining(","));
        return asKey("type") + quoted(getType()) + "," + asKey("days") + asArray(valuesJson) ;
    }

}

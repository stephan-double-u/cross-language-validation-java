package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Calendar;
import java.util.Date;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.quoted;

/*
 * Implementation note: The class is named {@code Dates} (with {@code s}) to avoid name clash with {@linkplain Date}.
 */
public abstract class Dates extends Constraint {

    private static final Logger log = LoggerFactory.getLogger(Dates.class);

    public static final int MILLIS_PER_DAY = 24 * 60 * 60 * 1000;

    Dates() {
    }

    @Override
    public boolean isSupportedType(final Class<?> clazz) {
        return  LocalDate.class.isAssignableFrom(clazz)
                || LocalDateTime.class.isAssignableFrom(clazz)
                || Calendar.class.isAssignableFrom(clazz)
                || Date.class.isAssignableFrom(clazz);
    }

    @Override
    public void validateValuesOrFail(final Class<?> ignore, final Class<?> ignoreToo) {
        final Integer minDays = (Integer) getValues().get(0);
        final Integer maxDays = (Integer) getValues().get(1);
        if (!getType().equals(Period.DAYS_TOKEN)
                && (minDays != null && minDays < 0 || maxDays != null && maxDays < 0)) {
            throw new IllegalArgumentException("'minDays' and 'maxDays' values must not be < 0");
        }
        if (minDays != null && maxDays != null && minDays > maxDays) {
            throw new IllegalArgumentException("'minDays' value must not be greater than 'maxDays' value");
        }
    }

    @Override
    public boolean validate(final Object dateObject, final Object ignored) {
        if (dateObject == null) {
            return true;
        }
        Integer minDays = (Integer) getValues().get(0);
        Integer maxDays = (Integer) getValues().get(1);
        // WTF? -> swap positive 'past days' values into matching negative values for easier validation
        // e.g. Past.minMaxDays(1,3) is the same as Period.minMaxDays(-3,-1)
        if (getType().equals(Past.DAYS_TOKEN)) {
            Integer minDaysCopy = minDays;
            minDays = maxDays != null ? -1 * maxDays : null;
            maxDays = minDaysCopy != null ? -1 * minDaysCopy : null;
        }
        final boolean match;
        if (dateObject instanceof LocalDate date) {
            match = validate(date, minDays, maxDays);
        } else if (dateObject instanceof LocalDateTime date) {
            match = validate(date, minDays, maxDays);
        } else if (dateObject instanceof Calendar date) {
            match = validate(date, minDays, maxDays);
        } else if (dateObject instanceof Date date) {
            match = validate(date, minDays, maxDays);
        } else {
            throw new IllegalArgumentException("Unsupported type: " + dateObject.getClass());
        }
        log.debug("Date '{}' is {}({}, {}): {}", dateObject, getType(), minDays, maxDays, match);
        return match;
    }

    private boolean validate(final LocalDate localDate, final Integer minDays, final Integer maxDays) {
        boolean match = true;
        LocalDate now = LocalDate.now();
            if (minDays != null) {
                match = now.plusDays(minDays).compareTo(localDate) <= 0;
            }
            if (maxDays != null) {
                match &= now.plusDays(maxDays).compareTo(localDate) >= 0;
            }
        return match;
    }

    private boolean validate(final LocalDateTime localDateTime, final Integer minDays, final Integer maxDays) {
        boolean match = true;
        LocalDateTime now = LocalDateTime.now();
            if (minDays != null) {
                match = now.plusDays(minDays).compareTo(localDateTime) <= 0;
            }
            if (maxDays != null) {
                match &= now.plusDays(maxDays).compareTo(localDateTime) >= 0;
            }
        return match;
    }

    private boolean validate(final Calendar calendar, final Integer minDays, final Integer maxDays) {
        final long millisDiffFromNow = calendar.getTime().getTime() - Calendar.getInstance().getTime().getTime();
        return validate(minDays, maxDays, millisDiffFromNow);
    }

    private boolean validate(final Date date, final Integer minDays, final Integer maxDays) {
        final Date now = new Date();
        final long millisDiffFromNow = date.getTime() - now.getTime();
        return validate(minDays, maxDays, millisDiffFromNow);
    }

    private boolean validate(Integer minDays, Integer maxDays, long millisDiffFromNow) {
        boolean match = true;
            if (minDays != null) {
                match = millisDiffFromNow - (minDays * MILLIS_PER_DAY) >= 0;
            }
            if (maxDays != null) {
                match &= millisDiffFromNow - (maxDays * MILLIS_PER_DAY) <= 0;
            }
        return match;
    }

    @Override
    public String serializeToJson() {
        final Integer min = (Integer) getValues().get(0);
        final Integer max = (Integer) getValues().get(1);
        String minJson = min != null ? asKey("min") + min : "";
        String maxJson = max != null ? asKey("max") + max : "";
        String seperator = min != null && max != null ? "," : "";
        return asKey("type") + quoted(getType()) + "," + minJson + seperator + maxJson;
    }

}

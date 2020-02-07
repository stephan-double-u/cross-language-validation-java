package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static de.swa.clv.json.JsonUtil.*;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

/**
 * Constraint to express expectations for dates.<br/>
 * In addition to {@link javax.validation.constraints.Future} and {@link javax.validation.constraints.Past} it is
 * possible to define how many days the date should be in the future resp. past.<br/>
 * And {@link LocalDate} is supported!<br/>
 * Implementation note: The class is named {@code Dates} (with {@code s}) to avoid name clash with
 * {@linkplain Date}.
 */
public class Dates extends ConstraintRoot {

    private static Logger log = LoggerFactory.getLogger(Dates.class);

    private static final String TYPE = "DATE";

    public static final String FUTURE_DATE_TOKEN = "FUTURE";
    public static final String PAST_DATE_TOKEN = "PAST";

    private Dates (final List<Object> values) {
        setObjectValues(values);
    }

    /**
     * The property that should be validated must be a date in the future. 'Now' is defined as the current time according
     * to the virtual machine. The calendar used if the compared type is of type {@code Calendar} is the calendar based
     * on the current timezone and the current locale.
     * <p/>
     * Supported property types are:
     * <ul>
     * <li>{@code java.time.LocalDate}</li>
     * <li>{@code java.util.Calendar}</li>
     * <li>{@code java.util.Date}</li>
     * </ul>
     * <p/>
     */
    public static Dates future() {
        return new Dates(Arrays.asList(FUTURE_DATE_TOKEN, 0));
    }

    public static Dates past() {
        return new Dates(Arrays.asList(PAST_DATE_TOKEN, 0));
    }

    // Extension compared to javax.validation.constraints.Future
    public static Dates future(int days) {
        return new Dates(Arrays.asList(FUTURE_DATE_TOKEN, days));
    }

    // Extension compared to javax.validation.constraints.Past
    public static Dates past(int days) {
        return new Dates(Arrays.asList(PAST_DATE_TOKEN, days));
    }

    @Override
    public boolean isSupportedType(final Class<?> clazz) {
        return  LocalDate.class.isAssignableFrom(clazz)
                || Calendar.class.isAssignableFrom(clazz)
                || Date.class.isAssignableFrom(clazz);
    }

    @Override
    public boolean validate(final Object dateObject, final Object ignored) {
        if (dateObject == null) {
            return true;
        }
        final String token = (String) getValues().get(0);
        final int daysOffset = (int) getValues().get(1);
        final long millisDaysOffset = daysOffset * 24 * 60 * 60 * 1000;
        final boolean match;
        if (dateObject instanceof LocalDate) {
            match = validate((LocalDate) dateObject, token, daysOffset);
        } else if (dateObject instanceof Calendar) {
            match = validate((Calendar) dateObject, token, millisDaysOffset);
        } else if (dateObject instanceof Date) {
            match = validate((Date) dateObject, token, millisDaysOffset);
        } else {
            throw new IllegalArgumentException("Unsupported type: " + dateObject.getClass());
        }
        log.debug("Date '{}' is {}({}): {}", dateObject, token, daysOffset, match);
        return match;
    }

    private boolean validate(final LocalDate localDate, final String token, final int daysOffset) {
        final boolean match;
        if (FUTURE_DATE_TOKEN.equals(token)) {
            match = LocalDate.now().plusDays(daysOffset).compareTo(localDate) < 0;
        } else {
            match = LocalDate.now().minusDays(daysOffset).compareTo(localDate) > 0;
        }
        return match;
    }

    private boolean validate(final Calendar calendar, final String token, final long millisDaysOffset) {
        final boolean match;
        final long millisDiff = Calendar.getInstance().getTime().getTime() - calendar.getTime().getTime();
        if (FUTURE_DATE_TOKEN.equals(token)) {
            match = millisDiff + millisDaysOffset < 0;
        } else {
            match = millisDiff - millisDaysOffset > 0;
        }
        return match;
    }

    private boolean validate(final Date date, final String token, final long millisDaysOffset) {
        final boolean match;
        final Date now = new Date();
        final long millisDiff = now.getTime() - date.getTime();
        if (FUTURE_DATE_TOKEN.equals(token)) {
            match = millisDiff + millisDaysOffset < 0;
        } else {
            match = millisDiff - millisDaysOffset > 0;
        }
        return match;
    }

    @Override
    public String serializeToJson() {
        String token = (String) getValues().get(0);
        int daysOffset = (int) getValues().get(1);
        return asKey("type") + quoted(TYPE + "_" + token) + "," + asKey("days") + daysOffset;
    }

    @Override
    public String getType() {
        return TYPE + "_" + getValues().get(0);
    }
}

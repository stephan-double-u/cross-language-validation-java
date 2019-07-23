package de.swa.easyvalidation.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static de.swa.easyvalidation.json.JsonUtil.*;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

/**
 * Constraint to express expectations for dates.<br/>
 * In addition to {@link javax.validation.constraints.Future} and {@link javax.validation.constraints.Past} it is
 * possible to define how many days the date should be in the future resp. past.<br/>
 * And {@link LocalDate} is supported! (TBD)<br/>
 * Implementation note: The class is named {@code Dates} (with {@code s}) to avoid name clash with
 * {@linkplain Date}.
 *
 * @author stwa1de
 *
 */
public class Dates extends Constraint {

    private static Logger log = LoggerFactory.getLogger(Dates.class);

    private static final String type = "DATE";

    private String messageDefault = "{validation.constraint.dates}";

    public static final String FUTURE_DATE_TOKEN = "FUTURE";
    public static final String PAST_DATE_TOKEN = "PAST";

    private Dates (final List<Object> values) {
        setObjectValues(values);
    }

    /**
     * The element that should be validated must be a date in the future. Now is defined as the current time according
     * to the virtual machine. The calendar used if the compared type is of type {@code Calendar} is the calendar based
     * on the current timezone and the current locale.
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code java.util.Date}</li>
     * <li>{@code java.util.Calendar}</li>
     * <li>{@code java.time.LocalDate} TODO support this type? Requires Java 8 ! ...</li>
     * </ul>
     * <p/>
     *
     * @author Stephan Wack
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
        return Date.class.isAssignableFrom(clazz)
                || Calendar.class.isAssignableFrom(clazz)
                || LocalDate.class.isAssignableFrom(clazz);
    }

    @Override
    public boolean validate(final Object object, final Object contraintObject) {
        if (object == null) {
            return true;
        }
        final String token = (String) getValues().get(0);
        final int daysOffset = (int) getValues().get(1);
        final long millisDaysOffset = daysOffset * 24 * 60 * 60 * 1000;
        final boolean match;
        if (object instanceof Date) {
            match = validate((Date) object, token, millisDaysOffset);
        } else if (object instanceof Calendar) {
            match = validate((Calendar) object, token, millisDaysOffset);
        } else if (object instanceof LocalDate) {
            match = validate((LocalDate) object, token, daysOffset);
        } else {
            throw new IllegalArgumentException("Unsupported type: " + object.getClass());
        }
        log.debug("Date '" + object + "' is " + token + "(" + daysOffset + "): " + match);
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

    private boolean validate(final Calendar calendar, final String token, final long millisDaysOffset) {
        final boolean match;
        final Calendar calDate = calendar;
        final long millisDiff = Calendar.getInstance().getTime().getTime() - calDate.getTime().getTime();
        if (FUTURE_DATE_TOKEN.equals(token)) {
            match = millisDiff + millisDaysOffset < 0;
        } else {
            match = millisDiff - millisDaysOffset > 0;
        }
        return match;
    }

    private boolean validate(final LocalDate object, final String token, final int daysOffset) {
        final boolean match;
        final LocalDate localDate = object;
        if (FUTURE_DATE_TOKEN.equals(token)) {
            match = LocalDate.now().plusDays(daysOffset).compareTo(localDate) < 0;
        } else {
            match = LocalDate.now().minusDays(daysOffset).compareTo(localDate) > 0;
        }
        return match;
    }

    @Override
    public String serializeToJson() {
        String token = (String) getValues().get(0);
        int daysOffset = (int) getValues().get(1);
        return asObject(asKey("type") + quoted(type) + "," + asKey("subType") + quoted(token) + "," + asKey("days") + daysOffset);
    }

}

package de.swa.clv.constraints;

import java.util.Arrays;
import java.util.List;

/**
 * Constraint to express expectations for future dates.
 */
public class Future extends Dates implements IsCreateConstraint, IsUpdateConstraint {

    private static final String DAYS_TOKEN = "FUTURE_DAYS";

    private Future(boolean nullEqualsTrue, List<Object> values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(values);
    }

    @Override
    public String getToken() {
        return DAYS_TOKEN;
    }

    /**
     * The property that should be validated must be a date in the future. 'Now' is defined as the current time
     * according to the virtual machine. The calendar used if the compared type is of type {@code Calendar} is the
     * calendar based on the current timezone and the current locale.<p/>
     * Supported property types are:
     * <ul>
     * <li>{@code java.time.LocalDate}</li>
     * <li>{@code java.util.Calendar}</li>
     * <li>{@code java.util.Date}</li>
     * </ul>
     * <p/>
     */
    public static Future minDays(int minDays) {
        return newFuture(false, minDays, null);
    }

    public static Future minDaysOrNull(int minDays) {
        return newFuture(true, minDays, null);
    }

    public static Future minMaxDays(int minDays, int maxDays) {
        return newFuture(false, minDays, maxDays);
    }

    public static Future minMaxDaysOrNull(int minDays, int maxDays) {
        return newFuture(true, minDays, maxDays);
    }

    private static Future newFuture(boolean nullEqualsTrue, int minDays, Integer maxDays) {
        Future constraint = new Future(nullEqualsTrue, Arrays.asList(minDays, maxDays));
        constraint.validateValuesOrFail(null, null);
        return constraint;
    }
}

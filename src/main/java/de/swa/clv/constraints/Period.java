package de.swa.clv.constraints;

import java.util.Arrays;
import java.util.List;

/**
 * Constraint to express expectations for date periods.
 */
public class Period extends Dates {

    public static final String DAYS_TOKEN = "PERIOD_DAYS";

    private Period(final List<Object> values) {
        setValues(values);
    }

    @Override
    public String getToken() {
        return DAYS_TOKEN;
    }

    /**
     * The property that should be validated must be a date. 'Now' is defined as the current time
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
    public static Period minDays(int minDays) {
        return newPeriod(minDays, null);
    }

    public static Period maxDays(int maxDays) {
        return newPeriod(null, maxDays);
    }

    public static Period minMaxDays(int minDays, int maxDays) {
        return newPeriod(minDays, maxDays);
    }

    private static Period newPeriod(Integer minDays, Integer maxDays) {
        final Period constraint = new Period(Arrays.asList(minDays, maxDays));
        constraint.validateValuesOrFail(null, null);
        return constraint;
    }
}

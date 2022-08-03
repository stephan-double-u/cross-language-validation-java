package de.swa.clv.constraints;

import java.util.Arrays;
import java.util.List;

/**
 * Constraint to express expectations for past dates.
 */
public class Past extends Dates {

    public static final String DAYS_TOKEN = "PAST_DAYS";

    private Past(final List<Object> values) {
        setObjectValues(values);
    }

    @Override
    public String getType() {
        return DAYS_TOKEN;
    }

    /**
     * The property that should be validated must be a date in the past. 'Now' is defined as the current time
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
    public static Past minDays(int minDays) {
        return newPast(minDays, null);
    }

    public static Past minMaxDays(int minDays, int maxDays) {
        return newPast(minDays, maxDays);
    }

    private static Past newPast(int minDays, Integer maxDays) {
        final Past constraint = new Past(Arrays.asList(minDays, maxDays));
        constraint.validateValuesOrFail(null, null);
        return constraint;
    }
}

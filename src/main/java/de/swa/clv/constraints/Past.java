package de.swa.clv.constraints;

import java.util.Arrays;
import java.util.List;

/**
 * Constraint to express expectations for past dates.
 */
public class Past extends Dates implements IsCreateConstraint, IsUpdateConstraint {

    public static final String DAYS_TOKEN = "PAST_DAYS";

    public Past(boolean nullEqualsTrue, List<Object> values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(values);
    }

    @Override
    public String getToken() {
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
        return newPast(false, minDays, null);
    }

    public static Past minDaysOrNull(int minDays) {
        return newPast(true, minDays, null);
    }

    public static Past minMaxDays(int minDays, int maxDays) {
        return newPast(false, minDays, maxDays);
    }

    public static Past minMaxDaysOrNull(int minDays, int maxDays) {
        return newPast(true, minDays, maxDays);
    }

    private static Past newPast(boolean nullEqualsTrue, int minDays, Integer maxDays) {
        Past constraint = new Past(nullEqualsTrue, Arrays.asList(minDays, maxDays));
        constraint.validateValuesOrFail(null, null);
        return constraint;
    }
}

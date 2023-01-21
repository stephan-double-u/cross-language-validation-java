package de.swa.clv.constraints;

import java.util.Arrays;
import java.util.List;

/**
 * Constraint to express expectations for date periods.
 */
public class Period extends Dates implements IsCreateConstraint, IsUpdateConstraint {

    public static final String DAYS_TOKEN = "PERIOD_DAYS";

    private Period(boolean nullEqualsTrue, List<Object> values) {
        setNullEqualsTrue(nullEqualsTrue);
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
        return newPeriod(false, minDays, null);
    }

    public static Period minDaysOrNull(int minDays) {
        return newPeriod(true, minDays, null);
    }

    public static Period maxDays(int maxDays) {
        return newPeriod(false, null, maxDays);
    }

    public static Period maxDaysOrNull(int maxDays) {
        return newPeriod(true, null, maxDays);
    }

    public static Period minMaxDays(int minDays, int maxDays) {
        return newPeriod(false, minDays, maxDays);
    }

    public static Period minMaxDaysOrNull(int minDays, int maxDays) {
        return newPeriod(true, minDays, maxDays);
    }

    private static Period newPeriod(boolean nullEqualsTrue, Integer minDays, Integer maxDays) {
        Period constraint = new Period(nullEqualsTrue, Arrays.asList(minDays, maxDays));
        constraint.validateValuesOrFail(null, null);
        return constraint;
    }
}

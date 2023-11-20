package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDateTime;
import java.time.Year;
import java.util.Arrays;
import java.util.List;

/**
 * Constraint to express expectations about the year of a date property.
 */
class YearRange extends Dates implements IsCreateConstraint, IsUpdateConstraint {

    private static final Logger log = LoggerFactory.getLogger(YearRange.class);

    public static final String TOKEN = "YEAR_RANGE";

    private RangeType rangeType;

    enum RangeType {ABSOLUTE, RELATIVE}

    YearRange(final boolean nullEqualsTrue, final Integer minValue, final Integer maxValue, RangeType type) {
        rangeType = type;
        setNullEqualsTrue(nullEqualsTrue);
        setValues(Arrays.asList(minValue, maxValue));
        validateValuesOrFail(null, null);
    }

    @Override
    public String getToken() {
        return TOKEN;
    }

    @Override
    public void validateValuesOrFail(final Class<?> ignore, final Class<?> ignoreToo) {
        if (getValues().size() != 2) {
            throw new IllegalArgumentException("YearRange needs 2 arguments");
        }
        final Integer min = (Integer) getValues().get(0);
        final Integer max = (Integer) getValues().get(1);
        if (min != null && max != null && min > max) {
            throw new IllegalArgumentException("YearRange min value must be min <= max value");
        }
    }

    @Override
    public boolean validate(final Object dateToValidate, final Object notRelevant) {
        if (dateToValidate == null) {
            log.debug("'Null object equals to {}", doesNullEqualsTrue());
            return doesNullEqualsTrue();
        }

        final int yearToValidate = getAsLocalDateTime(dateToValidate).getYear();
        final Integer min = (Integer) getValues().get(0);
        final Integer max = (Integer) getValues().get(1);

        Integer minAbs = min;
        Integer maxAbs = max;
        if (rangeType == RangeType.RELATIVE) {
            int currentYear = Year.now().getValue();
            minAbs = min != null ? min + currentYear : null;
            maxAbs = max != null ? max + currentYear : null;
        }
        boolean match = true;
        if (min != null) {
            match = yearToValidate >= minAbs;
        }
        if (max != null) {
            match &= yearToValidate <= maxAbs;
        }
        return match;
    }

    @Override
    public String serializeToJson() {
        return super.serializeToJson() + ",\"rangeType\":\"" + rangeType.name() + "\"";
    }
}

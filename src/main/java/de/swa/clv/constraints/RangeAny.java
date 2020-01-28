package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.Collections;

public class RangeAny extends RangeRoot {

    private static Logger log = LoggerFactory.getLogger(RangeAny.class);

    RangeAny() {
    }

    public <T extends Number & Comparable> RangeAny min(final T minValue) {
        setObjectValues(Arrays.asList(minValue));
        return this;
    }

    public <T extends Number & Comparable> RangeAny max(final T maxValue) {
        this.maxValues = Collections.unmodifiableList(Arrays.asList(maxValue));
        return this;
    }

    public <T extends Number & Comparable> RangeAny minAny(final T... minValues) {
        setObjectValues(Arrays.asList((Object[]) minValues));
        return this;
    }

    public <T extends Number, Comparable> RangeAny maxAny(final T... maxValues) {
        this.maxValues = Collections.unmodifiableList(Arrays.asList((Object[]) maxValues));
        return this;
    }

    @Override
    public boolean validate(final Object object, final Object notRelevant) {
        if (object == null) {
            return false;
        }
        boolean match = true;
        if (getValues() != null) {
            match &= getValues().stream().anyMatch(value -> ((Comparable) value).compareTo(object) <= 0);
        }
        if (getMaxValues() != null) {
            match &= getMaxValues().stream().anyMatch(value -> ((Comparable) value).compareTo(object) >= 0);
        }
        log.debug("'" + object + "' is" + (match ? "" : " NOT") + " within Range min " + getValues() + " and  max" + getMaxValues());
        return match;
    }

    @Override
    public String getType() {
        return "RANGE_ANY";
    }

}

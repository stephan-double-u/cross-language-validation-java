package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.invoke.MethodHandles;
import java.util.Arrays;
import java.util.Collections;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.quoted;

public class RangeStringAny extends RangeRoot {

    private static Logger log = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

    private ComparisonType comparisonType = ComparisonType.LEXICOGRAPHICAL_UNICODE;

    RangeStringAny() {
    }

    public RangeStringAny min(final String minValue) {
        setObjectValues(Arrays.asList(minValue));
        return this;
    }

    public RangeStringAny max(final String maxValue) {
        this.maxValues = Collections.unmodifiableList(Arrays.asList(maxValue));
        return this;
    }

    public RangeStringAny minAny(final String... minValues) {
        setObjectValues(Arrays.asList(minValues));
        return this;
    }

    public RangeStringAny maxAny(final String... maxValues) {
        this.maxValues = Collections.unmodifiableList(Arrays.asList(maxValues));
        return this;
    }

    public RangeRoot use(ComparisonType comparisonType) {
        this.comparisonType = comparisonType;
        return this;
    }

    @Override
    public String getType() {
        return "RANGE_ANY";
    }

    @Override
    public boolean validate(final Object object, final Object notRelevant) {
        if (object == null) {
            return false;
        }
        boolean match = true;
        if (getValues() != null) {
            match &= getValues().stream().anyMatch(value -> compare(value, object) <= 0);
        }
        if (getMaxValues() != null) {
            match &= getMaxValues().stream().anyMatch(value -> compare(value, object) >= 0);
        }
        log.debug("'" + object + "' is" + (match ? "" : " NOT") + " within Range min " + getValues() + " and  max" + getMaxValues());
        return match;
    }

    private int compare(Object value, Object object) {
        if (comparisonType == ComparisonType.LEXICOGRAPHICAL_UNICODE) {
            return ((String) value).compareTo((String) object);
        }
        return ((String) value).toLowerCase().compareTo(((String) object).toLowerCase());
    }


    @Override
    public String serializeToJson() {
        return super.serializeToJson() + "," + asKey("comparisonType") + quoted(comparisonType.name());
    }


}

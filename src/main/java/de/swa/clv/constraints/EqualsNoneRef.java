package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EqualsNoneRef extends EqualsRef {

    private static final Logger log = LoggerFactory.getLogger(EqualsNoneRef.class);

    public static final String TOKEN = "EQUALS_NONE_REF";

    EqualsNoneRef(final boolean nullEqualsTrue, String... values) {
        setObjectValues(getValuesWithAllowFlagAsObjectList(nullEqualsTrue, values));
    }

    @Override
    public String getType() {
        return TOKEN;
    }

    @Override
    public boolean validate(final Object valueToValidate, final Object constraintObject) {
        final Boolean nullEqualsTrue = (Boolean) getValues().get(0);
        if (valueToValidate == null) {
            log.debug("'Null object equals to {}", nullEqualsTrue);
            return nullEqualsTrue;
        }
        return !getValues().stream()
                .skip(1)
                .map(refProperty -> validateRefProperty((String) refProperty, valueToValidate, constraintObject))
                .filter(e -> e)
                .findFirst().orElse(false);
    }

}

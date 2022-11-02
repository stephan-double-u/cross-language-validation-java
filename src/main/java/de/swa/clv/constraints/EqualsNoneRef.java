package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EqualsNoneRef extends EqualsRef {

    private static final Logger log = LoggerFactory.getLogger(EqualsNoneRef.class);

    public static final String TOKEN = "EQUALS_NONE_REF";

    EqualsNoneRef(final boolean nullEqualsTrue, String... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    @Override
    public String getToken() {
        return TOKEN;
    }

    @Override
    public boolean validate(final Object valueToValidate, final Object constraintObject) {
        if (valueToValidate == null) {
            log.debug("'Null object equals to {}", doesNullEqualsTrue());
            return doesNullEqualsTrue();
        }
        return !getValues().stream()
                .map(refProperty -> validateReferencedProperty((String) refProperty, valueToValidate, constraintObject))
                .filter(e -> e)
                .findFirst().orElse(false);
    }

}

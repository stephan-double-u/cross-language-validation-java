package de.swa.clv.constraints;

import java.time.LocalDateTime;
import java.time.temporal.IsoFields;
import java.util.List;

/**
 * Constraint to express the expectation that a date must be in at least one of the quarters given by the values of the
 * listed properties.
 */
public class QuarterAnyRef extends Quarter implements ReferenceProperties {

    public static final String TOKEN = "QUARTER_ANY_REF";

    QuarterAnyRef(final boolean nullEqualsTrue, final String... properties) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(properties));
    }

    @Override
    public String getToken() {
        return TOKEN;
    }

    @Override
    public void validateValuesOrFail(final Class<?> typeClass, final Class<?> ignoredPropertyType) {
        validateReferencedTypesOrFail(getValues(), typeClass, Integer.class);
    }

    @Override
    public boolean validate(final Object valueToValidate, final Object constraintObject) {
        if (valueToValidate == null) {
            log.debug("'Null object equals to {}", doesNullEqualsTrue());
            return doesNullEqualsTrue();
        }
        return getValues().stream()
                .map(refProperty -> validateReferencedProperty((String) refProperty, valueToValidate, constraintObject))
                .filter(e -> e)
                .findFirst().orElse(false);
    }

    @Override
    public boolean validateReferencedValue(Object dateToValidate, Object value) {
        LocalDateTime dateAsLocalDateTime = getAsLocalDateTime(dateToValidate);
        Integer dateQuarter = dateAsLocalDateTime.get(IsoFields.QUARTER_OF_YEAR);
        return Equals.equals(dateQuarter, value);
    }
}

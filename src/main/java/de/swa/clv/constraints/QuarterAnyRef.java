package de.swa.clv.constraints;

import java.time.LocalDateTime;
import java.time.temporal.IsoFields;

/**
 * Constraint to express the expectation that a date must be in at least one of the quarters given by the values of the
 * listed properties.
 */
public class QuarterAnyRef extends DatesRef implements IsCreateConstraint, IsUpdateConstraint,
        ReferenceProperties<QuarterAnyRef> {

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
        validateReferencedTypesOrFail(getValues(), typeClass, Number.class);
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
        return Equals.equalsUntyped(dateQuarter, value);
    }

    @Override
    public QuarterAnyRef ofUpdate() {
        setOfUpdate(true);
        return this;
    }

    @Override
    public QuarterAnyRef ofCurrent() {
        setOfCurrent(true);
        return this;
    }

    @Override
    public String serializeToJson() {
        return serializeToJsonAsValuesArray();
    }

}

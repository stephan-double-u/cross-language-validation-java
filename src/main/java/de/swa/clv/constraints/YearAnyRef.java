package de.swa.clv.constraints;

import java.time.LocalDateTime;

/**
 * Constraint to express the expectation that a date year must equal at least one of the years given by the values of
 * the listed properties.
 */
public class YearAnyRef extends DatesRef implements IsCreateConstraint, IsUpdateConstraint,
        ReferenceProperties<YearAnyRef> {

    public static final String TOKEN = "YEAR_ANY_REF";

    YearAnyRef(final boolean nullEqualsTrue, final String... properties) {
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
        return EqualsConstraint.equalsUntyped(dateAsLocalDateTime.getYear(), value);
    }

    @Override
    public YearAnyRef ofUpdate() {
        setOfUpdate(true);
        return this;
    }

    @Override
    public YearAnyRef ofCurrent() {
        setOfCurrent(true);
        return this;
    }

    @Override
    public String serializeToJson() {
        return serializeToJsonAsValuesArray();
    }

}

package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EqualsAnyRef extends EqualsRef implements IsCreateConstraint, IsUpdateConstraint,
        ReferenceProperties<EqualsAnyRef> {

    private static final Logger log = LoggerFactory.getLogger(EqualsAnyRef.class);

    public static final String TOKEN = "EQUALS_ANY_REF";

    EqualsAnyRef(final boolean nullEqualsTrue, String... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    @Override
    public String getToken() {
        return TOKEN;
    }

    @Override
    public void validateValuesOrFail(final Class<?> typeClass, final Class<?> propertyType) {
        validateReferencedTypesOrFail(getValues(), typeClass, propertyType);
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
    public EqualsAnyRef ofUpdate() {
        setOfUpdate(true);
        return this;
    }

    @Override
    public EqualsAnyRef ofCurrent() {
        setOfCurrent(true);
        return this;
    }
}

package de.swa.clv.constraints;

import java.util.Arrays;

public class EqualsAnyRef extends EqualsRef {

    EqualsAnyRef(String... properties) {
        setObjectValues(Arrays.asList(properties));
    }

    @Override
    public String getType() {
        return "EQUALS_ANY_REF";
    }

    @Override
    public boolean validate(final Object valueToValidate, final Object constraintObject) {
        return getValues().stream()
                .map(refProperty -> validateRefProperty((String) refProperty, valueToValidate, constraintObject))
                .filter(e -> e)
                .findFirst().orElse(false);
    }

}

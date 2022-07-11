package de.swa.clv.constraints;

import java.util.Arrays;

public class EqualsNoneRef extends EqualsRef {

    EqualsNoneRef(String... properties) {
        setObjectValues(Arrays.asList(properties));
    }

    @Override
    public String getType() {
        return "EQUALS_NONE_REF";
    }

    @Override
    public boolean validate(final Object valueToValidate, final Object constraintObject) {
        return !getValues().stream()
                .map(refProperty -> validateRefProperty((String) refProperty, valueToValidate, constraintObject))
                .filter(e -> e)
                .findFirst().orElse(false);
    }

}

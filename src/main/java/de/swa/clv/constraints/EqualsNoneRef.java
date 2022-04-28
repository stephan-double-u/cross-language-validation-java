package de.swa.clv.constraints;

import java.util.Arrays;

import de.swa.clv.Validator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EqualsNoneRef extends EqualsRef {

    EqualsNoneRef(String... properties) {
        setObjectValues(Arrays.asList((Object[]) properties));
    }

    @Override
    public String getType() {
        return "EQUALS_NONE_REF";
    }

    @Override
    public boolean validate(final Object valueToValidate, final Object constraintObject) {
        if (valueToValidate == null) {
            return false;
        }

        return !getValues().stream()
                .map(refProperty -> validateSingleRefProperty((String) refProperty, valueToValidate, constraintObject))
                .filter(e -> e)
                .findFirst().orElse(false);
    }

}

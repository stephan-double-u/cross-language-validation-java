package de.swa.clv.constraints;

import de.swa.clv.AggregateFunction;
import de.swa.clv.Validator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class EqualsAnyRef extends EqualsRef {

    EqualsAnyRef(String... properties) {
        setObjectValues(Arrays.asList((Object[]) properties));
    }

    @Override
    public String getType() {
        return "EQUALS_ANY_REF";
    }

    @Override
    public boolean validate(final Object valueToValidate, final Object constraintObject) {
        if (valueToValidate == null) {
            return false;
        }

        return getValues().stream()
                .map(refProperty -> validateSingleRefProperty((String) refProperty, valueToValidate, constraintObject))
                .filter(e -> e)
                .findFirst().orElse(false);
    }

}

package de.swa.clv.constraints;

import java.util.Arrays;

import de.swa.clv.Validator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EqualsNoneRef extends EqualsRoot {

    private static final Logger log = LoggerFactory.getLogger(EqualsNoneRef.class);

    EqualsNoneRef(String... properties) {
        setObjectValues(Arrays.asList((Object[]) properties));
    }

    @Override
    public String getType() {
        return "EQUALS_NONE_REF";
    }

    @Override
    public boolean validateValuesOrFail(final Class<?> propertyType) {
        getValues().forEach(refPropertyName -> Validator.instance().validateProperty((String) refPropertyName, propertyType));
        return true;
    }

    @Override
    public boolean validate(final Object valueToValidate, final Object constraintObject) {
        if (valueToValidate == null) {
            return false;
        }

        final boolean equals = getValues().stream()
                .flatMap(property -> Validator.instance().inflatePropertyIfIndexed((String) property, constraintObject).stream())
                .map(property -> Validator.instance().getPropertyResultObject(property, constraintObject))
                .map(referencedValue -> EqualsRoot.equals(valueToValidate, referencedValue))
                .filter(e -> e)
                .findFirst().orElse(false);

        log.debug("" + valueToValidate + (equals ? " " : " NOT ") + "equals a referenced property of " + getValues());
        return !equals;
    }

}

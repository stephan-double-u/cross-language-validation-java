package de.swa.clv.constraints;

import java.util.Arrays;

import de.swa.clv.EasyValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EqualsNoneRef extends EqualsRoot {

    private static Logger log = LoggerFactory.getLogger(EqualsNoneRef.class);

    EqualsNoneRef(String... properties) {
        setObjectValues(Arrays.asList((Object[]) properties));
    }

    @Override
    public String getType() {
        return "EQUALS_NONE_REF";
    }

    @Override
    public boolean validateValuesOrFail(final Class<?> propertyType) {
        getValues().forEach(refPropertyName -> EasyValidator.validateProperty((String) refPropertyName, propertyType));
        return true;
    }

    @Override
    public boolean validate(final Object valueToValidate, final Object constraintObject) {
        if (valueToValidate == null) {
            return false;
        }

        final Boolean equals = getValues().stream()
                .map(property -> EasyValidator.getPropertyResultObject((String) property, constraintObject))
                .map(referencedValue -> EqualsRoot.equals(valueToValidate, referencedValue))
                .filter(e -> e == true)
                .findFirst().orElse(false);

        log.debug("" + valueToValidate + (equals ? " " : " not ") + "equals a referenced property of " + getValues());
        return !equals;
    }

}

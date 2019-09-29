package de.swa.easyvalidation.constraints;

import java.util.Arrays;

import de.swa.easyvalidation.EasyValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EqualsAnyRef extends Equals {

    private static Logger log = LoggerFactory.getLogger(EqualsAnyRef.class);

    EqualsAnyRef(String... properties) {
        setObjectValues(Arrays.asList((Object[]) properties));
    }

    @Override
    String getType() {
        return "EQUALS_ANY_REF";
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
                .map(referencedValue -> Equals.equals(valueToValidate, referencedValue))
                .filter(e -> e == true)
                .findFirst().orElse(false);

        log.debug("" + valueToValidate + (equals ? " " : " NOT ") + "equals a referenced property of " + getValues());
        return equals;
    }

}

package de.swa.easyvalidation.constraints;

import java.util.Arrays;

import de.swa.easyvalidation.EasyValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EqualsNoneRef extends Equals {

    private static Logger log = LoggerFactory.getLogger(EqualsNoneRef.class);

    EqualsNoneRef(String... properties) {
        setStringValues(Arrays.asList(properties));
    }

    @Override
    String getType() {
        return "EQUALS_NONE_REF";
    }

    @Override
    public boolean validateArgumentsOrFail(final Class<?> typeClass) {
        getValues().forEach(refPropertyName -> EasyValidator.validateProperty((String) refPropertyName, typeClass));
        return true;
    }

    @Override
    public boolean validate(final Object valueToValidate, final Object contraintObject) {
        for (final Object property : getValues()) {
            //TODO allow only non-indexed properties resp. 'single index'!?
            final Object referencedValue = EasyValidator.getPropertyResultObject((String) property, contraintObject);
            if (valueToValidate.equals(referencedValue)) {
                log.debug("" + valueToValidate + " does NOT equals none referenced property " + property);
                return false;
            }
        }
        log.debug("" + valueToValidate + " does equals none referenced property of " + getValues());
        return true;
    }

}

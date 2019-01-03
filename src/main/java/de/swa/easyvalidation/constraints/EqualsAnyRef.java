package de.swa.easyvalidation.constraints;

import java.util.Arrays;

import de.swa.easyvalidation.EasyValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EqualsAnyRef extends Equals {

    private static Logger log = LoggerFactory.getLogger(EqualsAnyRef.class);

    EqualsAnyRef(String... properties) {
        setStringValues(Arrays.asList(properties));
    }

    @Override
    String getType() {
        return "EQUALS_ANY_REF";
    }

    @Override
    public boolean validateArgumentsOrFail(Class<?> typeClass) {
        getValues().forEach(refPropertyName -> EasyValidator.validateProperty((String) refPropertyName, typeClass));
        return true;
    }
    
    @Override
    public boolean validate(Object valueToValidate, Object contraintObject) {
        for (Object property : getValues()) {
            //TODO allow only non-indexed properties resp. 'single index'!?
            Object referencedValue = EasyValidator.getPropertyResultObject((String) property, contraintObject);
            if (valueToValidate.equals(referencedValue)) {
                log.debug("" + valueToValidate + " does equals referenced property " + property);
                return true;
            }
        }
        log.debug("" + valueToValidate + " does NOT equals any referenced property of " + getValues());
        return false;
    }

}

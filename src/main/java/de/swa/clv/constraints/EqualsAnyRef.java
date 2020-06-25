package de.swa.clv.constraints;

import de.swa.clv.Validator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;

public class EqualsAnyRef extends EqualsRoot {

    private static Logger log = LoggerFactory.getLogger(EqualsAnyRef.class);

    EqualsAnyRef(String... properties) {
        setObjectValues(Arrays.asList((Object[]) properties));
    }

    @Override
    public String getType() {
        return "EQUALS_ANY_REF";
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

        final Boolean equals = getValues().stream()
                .flatMap(property -> Validator.instance().inflatePropertyIfIndexed((String) property, constraintObject).stream())
                .map(property -> Validator.instance().getPropertyResultObject(property, constraintObject))
                .map(referencedValue -> EqualsRoot.equals(valueToValidate, referencedValue))
                .filter(e -> e == true)
                .findFirst().orElse(false);

        log.debug("" + valueToValidate + (equals ? " " : " NOT ") + "equals a referenced property of " + getValues());
        return equals;
    }

}

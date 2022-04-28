package de.swa.clv.constraints;

import de.swa.clv.AggregateFunction;
import de.swa.clv.Validator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;

public abstract class EqualsRef extends EqualsRoot {

    private static final Logger log = LoggerFactory.getLogger(EqualsRef.class);

    @Override
    public boolean isSupportedType(Class<?> clazz) {
        return true;
    }

    @Override
    public boolean validateValuesOrFail(final Class<?> propertyType) {
         boolean typesDoNotMatch  = getValues().stream()
                 .map(refProperty -> ((String) refProperty).split("#")[0])
                 .map(pureProperty -> Validator.instance().validateProperty(pureProperty, propertyType))
                 .anyMatch(valueRefType -> propertyType != valueRefType);
        return !typesDoNotMatch;
    }

    boolean validateSingleRefProperty(String refProperty, Object valueToValidate, Object constraintObject) {
        AggregateFunction aggregateFunction = Validator.instance().validateAndGetTerminalAggregateFunctionIfExist(
                refProperty).orElseGet(() -> null);
        String pureProperty = refProperty.split("#")[0];
        List<String> propertiesToCheck = Validator.instance().inflatePropertyIfIndexed(pureProperty, constraintObject);
        boolean equals = false;
        if (aggregateFunction != null) {
            switch(aggregateFunction) {
            case sum:
                BigDecimal sum = Validator.instance().sumUpPropertyValues(constraintObject, propertiesToCheck);
                equals = sum.equals(new BigDecimal(valueToValidate.toString()));
                break;
            case distinct:
                Boolean distinct = Validator.instance().distinctCheckForPropertyValues(constraintObject, propertiesToCheck);
                equals = distinct.equals(valueToValidate);
                break;
            default:
                throw new IllegalArgumentException("Should not happen. Unsupported: " + aggregateFunction);
            }
        } else {
            equals = propertiesToCheck.stream()
                    .map(property -> Validator.instance().getPropertyResultObject(property, constraintObject))
                    .map(referencedValue -> EqualsRoot.equals(valueToValidate, referencedValue))
                    .filter(e -> e)
                    .findFirst().orElse(false);

        }
        log.debug("" + valueToValidate + (equals ? " " : " NOT ") + "equals referenced property " + refProperty);
        return equals;
    }

}

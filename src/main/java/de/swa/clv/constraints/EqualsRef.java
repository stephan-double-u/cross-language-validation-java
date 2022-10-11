package de.swa.clv.constraints;

import de.swa.clv.AggregateFunction;
import de.swa.clv.Validator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

public abstract class EqualsRef extends Equals {

    private static final Logger log = LoggerFactory.getLogger(EqualsRef.class);

    @Override
    public boolean isSupportedType(Class<?> clazz) {
        return true;
    }

    @Override
    public void validateValuesOrFail(final Class<?> typeClass, final Class<?> propertyType) {
         getValues().stream()
                 .skip(1) // boolean nullEqualsTrue
                 .filter(Objects::nonNull)
                 .forEach(refProperty  -> validateValueOrFail(typeClass, propertyType, (String) refProperty));
    }

    private void validateValueOrFail(Class<?> typeClass, Class<?> propertyType, String refProperty) {
        Class<?> valueRefType = getValueRefType(typeClass, refProperty);
        if (valueRefType != propertyType) {
           throw new IllegalArgumentException("Type of referenced property is " + valueRefType +
                   " but must be " + propertyType);
        }
    }

    private Class<?> getValueRefType(Class<?> typeClass, String refProperty) {
        String pureProperty = refProperty.split("#")[0];
        Class<?> valueRefType = Validator.instance().validateProperty(pureProperty, typeClass);
        Optional<AggregateFunction> aggregateFunction = Validator.instance()
                .validateAndGetTerminalAggregateFunctionIfExist(refProperty);
        if (aggregateFunction.isPresent() && aggregateFunction.get().equals(AggregateFunction.distinct)) {
            valueRefType = Boolean.class;
        }
        return valueRefType;
    }

    boolean validateRefProperty(String refProperty, Object valueToValidate, Object constraintObject) {
        if (refProperty == null) {
            boolean valueIsNull = valueToValidate == null;
            log.debug("A null refProperty validated against a {}null value is considered as '{}'",
                    valueIsNull ? "" : "NOT ", valueIsNull);
            return valueIsNull;
        }
        AggregateFunction aggregateFunction = Validator.instance().validateAndGetTerminalAggregateFunctionIfExist(
                refProperty).orElseGet(() -> null);
        String pureProperty = refProperty.split("#")[0];
        List<String> propertiesToCheck = Validator.instance().inflatePropertyIfIndexed(pureProperty, constraintObject);
        boolean equals;
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
                    .map(referencedValue -> Equals.equals(valueToValidate, referencedValue))
                    .filter(e -> e)
                    .findFirst().orElse(false);

        }
        log.debug("" + valueToValidate + (equals ? " " : " NOT ") + "equals referenced property " + refProperty);
        return equals;
    }

}

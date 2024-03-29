package de.swa.clv.constraints;

import de.swa.clv.AggregateFunction;
import de.swa.clv.Validator;
import de.swa.clv.util.TypeHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

public interface ReferenceProperties<T> {

    Logger log = LoggerFactory.getLogger(ReferenceProperties.class);

    boolean validateReferencedValue(Object valueToValidate, Object referencedValue);

    T ofUpdate();

    boolean isOfUpdate();

    T ofCurrent();

    boolean isOfCurrent();

    default void validateReferencedTypesOrFail(final List<Object> refProperty, final Class<?> typeClass,
            final Class<?> propertyType) {
        refProperty.forEach(p  -> validateRefTypeOrFail(typeClass, propertyType, (String) p));
    }

    private void validateRefTypeOrFail(Class<?> typeClass, Class<?> propertyType, String refProperty) {
        Class<?> valueRefType = getRefType(typeClass, refProperty);
        final Class<?> wrappedClass = (valueRefType.isPrimitive())
                ? TypeHelper.PRIMITIVE_TO_WRAPPER_TYPES.get(valueRefType) : valueRefType;

        if (Number.class.isAssignableFrom(wrappedClass) && !Number.class.isAssignableFrom(propertyType)
            || !Number.class.isAssignableFrom(wrappedClass) && wrappedClass != propertyType) {
            throw new IllegalArgumentException("Type of referenced property is " + valueRefType +
                    " but must be " + propertyType);
        }
    }

    private Class<?> getRefType(Class<?> typeClass, String refProperty) {
        String pureProperty = refProperty.split("#")[0];
        Class<?> valueRefType = Validator.instance().validateProperty(pureProperty, typeClass);
        Optional<AggregateFunction> aggregateFunction = Validator.instance()
                .validateAndGetTerminalAggregateFunctionIfExist(refProperty);
        if (aggregateFunction.isPresent() && aggregateFunction.get().equals(AggregateFunction.distinct)) {
            valueRefType = Boolean.class;
        }
        return valueRefType;
    }

    default boolean validateReferencedProperty(String refProperty, Object valueToValidate, Object constraintObject) {
        AggregateFunction aggregateFunction = Validator.instance().validateAndGetTerminalAggregateFunctionIfExist(
                refProperty).orElseGet(() -> null);
        String pureProperty = refProperty.split("#")[0];
        List<String> propertiesToCheck = Validator.instance().inflatePropertyIfIndexed(pureProperty, constraintObject);
        boolean equals;
        if (aggregateFunction != null) {
            switch (aggregateFunction) {
            case sum -> {
                BigDecimal sum = Validator.instance().sumUpPropertyValues(constraintObject, propertiesToCheck);
                equals = validateReferencedValue(new BigDecimal(valueToValidate.toString()), sum);
            }
            case distinct -> {
                Boolean distinct = Validator.instance()
                        .distinctCheckForPropertyValues(constraintObject, propertiesToCheck);
                equals = validateReferencedValue(valueToValidate, distinct);
            }
            default -> throw new IllegalArgumentException("Should not happen. Unsupported: " + aggregateFunction);
            }
        } else {
            equals = propertiesToCheck.stream()
                    .map(property -> Validator.instance().getPropertyResultObject(property, constraintObject))
                    .map(referencedValue -> validateReferencedValue(valueToValidate, referencedValue))
                    .filter(e -> e)
                    .findFirst().orElse(false);
        }
        log.debug("" + valueToValidate + (equals ? " " : " NOT ") + "equals referenced property " + refProperty);
        return equals;
    }
}

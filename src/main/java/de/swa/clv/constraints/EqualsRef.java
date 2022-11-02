package de.swa.clv.constraints;

import de.swa.clv.AggregateFunction;
import de.swa.clv.Validator;
import de.swa.clv.util.TypeHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.temporal.IsoFields;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

public abstract class EqualsRef extends Equals  implements ReferenceProperties {

    private static final Logger log = LoggerFactory.getLogger(EqualsRef.class);

    @Override
    public boolean isSupportedType(Class<?> clazz) {
        return true;
    }

    @Override
    public void validateValuesOrFail(final Class<?> typeClass, final Class<?> propertyType) {
        validateReferencedTypesOrFail(getValues(), typeClass, propertyType);
    }

    @Override
    public boolean validateReferencedValue(Object objectToValidate, Object value) {
        return Equals.equals(objectToValidate, value);
    }

}

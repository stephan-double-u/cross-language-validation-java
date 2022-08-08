package de.swa.clv.constraints;

import de.swa.clv.util.TypeHelper;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;

import static de.swa.clv.json.JsonUtil.*;

public abstract class EqualsRoot extends ConstraintRoot {

    EqualsRoot() {
    }

    /**
     * Enums should be compared based on their names, e.g. Enum.FOO should be equal to "FOO" and vice versa.
     *
     * @param objectToValidate the 1st object to compare
     * @param value the 2nd object to compare
     * @return {@code true} if the objects (resp. their names for enums) are equal, otherwise  {@code false}
     */
    static boolean equals(Object objectToValidate, Object value) {
        if (objectToValidate == null && value == null) {
            return true;
        }
        if (objectToValidate == null || value == null) {
            return false;
        }
        if (Enum.class.isAssignableFrom(objectToValidate.getClass())) {
            objectToValidate = ((Enum<?>) objectToValidate).name();
        }
        if (Enum.class.isAssignableFrom(value.getClass())) {
            value = ((Enum<?>) value).name();
        }
        if (objectToValidate instanceof BigDecimal) {
            return new BigDecimal(objectToValidate.toString()).equals(new BigDecimal(value.toString()));
        } else {
            return objectToValidate.equals(value);
        }
    }

    @Override
    public boolean isSupportedType(Class<?> clazz) {
        final Class<?> wrappedClass = (clazz.isPrimitive()) ? TypeHelper.PRIMITIVE_TO_WRAPPER_TYPES.get(clazz) : clazz;
        // EqualsNull and EqualsNotNull have no values; 1st value of other constraints in 'nullEqualsTrue'-boolean
        Class<?> valueClass = getValues() != null ? getValues().get(1).getClass() : null;
        //TODO allow LocalDateTime
        return (String.class == wrappedClass || Enum.class.isAssignableFrom(clazz))
                && (valueClass == null || String.class == valueClass || Enum.class.isAssignableFrom(valueClass))
                || (Boolean.class == wrappedClass // Boolean is final
                || Number.class.isAssignableFrom(wrappedClass)
                || LocalDate.class == wrappedClass  // LocalDate is final
                || Date.class.isAssignableFrom(wrappedClass))
                && (valueClass == null || wrappedClass == valueClass);
    }

    @Override
    public String serializeToJson() {
        String nullEqualsTrueJson = "";
        Boolean nullEqualsTo = (Boolean) getValues().get(0);
        // Serialize "nullEqualsTo" key only for non-default values,
        // i.e. 'true' for EqualsAny and EqualsAnyRef resp. 'false' for EqualsNONE and EqualsNoneRef
        if (nullEqualsTo.equals(Boolean.TRUE)
                && (getType().equals(EqualsAny.TOKEN) || getType().equals(EqualsAnyRef.TOKEN))
                || nullEqualsTo.equals(Boolean.FALSE)
                && (getType().equals(EqualsNone.TOKEN) || getType().equals(EqualsNoneRef.TOKEN))) {
            nullEqualsTrueJson = "," + asKey("nullEqualsTo") + nullEqualsTo;
        }
        String valuesJson = "," + asKey("values") + asArray(getValues().subList(1, getValues().size()));
        return asKey("type") + quoted(getType()) + valuesJson + nullEqualsTrueJson;
    }

}

package de.swa.clv.constraints;

import de.swa.clv.util.TypeHelper;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Date;
import java.util.Objects;

import static de.swa.clv.json.JsonUtil.*;

public abstract class EqualsConstraint extends Constraint {

    EqualsConstraint() {
    }

    /**
     * Enums are compared based on their names, e.g. Enum.FOO is considered equal to "FOO" and vice versa.
     * Numbers are compared numerically, i.e. e.g. {@code equalsUntyped(1e1, BigInteger.TEN)} is considered equal.
     *
     * @param o1 the 1st object to compare
     * @param o2 the 2nd object to compare
     * @return {@code true} if the objects are considered equal, otherwise  {@code false}
     */
    static boolean equalsUntyped(Object o1, Object o2) {
        if (o1 == null && o2 == null) {
            return true;
        }
        if (o1 == null || o2 == null) {
            return false;
        }
        if (Enum.class.isAssignableFrom(o1.getClass())) {
            o1 = ((Enum<?>) o1).name();
        }
        if (Enum.class.isAssignableFrom(o2.getClass())) {
            o2 = ((Enum<?>) o2).name();
        }
        if (Number.class.isAssignableFrom(o1.getClass())
                && Number.class.isAssignableFrom(o2.getClass())) {
            return equalsUntyped((Number) o1, (Number) o2);
        } else {
            return Objects.equals(o1, o2);
        }
    }

    private static boolean equalsUntyped(Number n1, Number n2) {
        return new BigDecimal(n1.toString()).compareTo(new BigDecimal(n2.toString())) == 0;
    }

    @Override
    public boolean isSupportedType(Class<?> clazz) {
        final Class<?> wrappedClass = (clazz.isPrimitive()) ? TypeHelper.PRIMITIVE_TO_WRAPPER_TYPES.get(clazz) : clazz;
        // EqualsNull and EqualsNotNull have no values
        Class<?> valueClass = getValues() != null ? getValues().get(0).getClass() : null;
        //TODO allow LocalDateTime
        return (String.class == clazz || Enum.class.isAssignableFrom(clazz))
                && (valueClass == null || String.class == valueClass || Enum.class.isAssignableFrom(valueClass))
                || (Boolean.class == wrappedClass // Boolean is final
                || Number.class.isAssignableFrom(wrappedClass)
                || LocalDate.class == wrappedClass  // LocalDate is final
                || Date.class.isAssignableFrom(wrappedClass))
                && (valueClass == null || wrappedClass == valueClass);
    }

    @Override
    public String serializeToJson() {
        String nullEqualsToJson = "";
        // Serialize "nullEqualsTo" key only for non-default values,
        // i.e. 'true' for EqualsAny and EqualsAnyRef resp. 'false' for EqualsNONE and EqualsNoneRef
        if (doesNullEqualsTrue()
                && (getToken().equals(EqualsAny.TOKEN) || getToken().equals(EqualsAnyRef.TOKEN))
                || !doesNullEqualsTrue()
                && (getToken().equals(EqualsNone.TOKEN) || getToken().equals(EqualsNoneRef.TOKEN))) {
            nullEqualsToJson = "," + asKey("nullEqualsTo") + doesNullEqualsTrue();
        }
        String refTargetJson = serializeRefTargetKeyValuePairForReferenceProperties();
        String valuesJson = "," + asKey("values") + asArray(getValues());
        return asKey("type") + quoted(getToken()) + valuesJson + nullEqualsToJson + refTargetJson;
    }

}

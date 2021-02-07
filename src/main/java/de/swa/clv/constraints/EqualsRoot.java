package de.swa.clv.constraints;

import de.swa.clv.util.TypeHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDate;

import static de.swa.clv.json.JsonUtil.*;

public abstract class EqualsRoot extends ConstraintRoot {

    private static Logger log = LoggerFactory.getLogger(EqualsRoot.class);

    EqualsRoot() {
    }

    /**
     * Enums should be compared based on their names, e.g. Enum.FOO should be equal to "FOO" and vice versa.
     *
     * @param valueToValidate the 1st object to compare
     * @param referencedValue the 2nd object to compare
     * @return {@code true} if the objects (resp. their names for enums) are equal, otherwise  {@code false}
     */
    static boolean equals(Object valueToValidate, Object referencedValue) {
        if (valueToValidate == null && referencedValue == null) {
            return true;
        }
        if (valueToValidate == null || referencedValue == null) {
            return false;
        }
        if (Enum.class.isAssignableFrom(valueToValidate.getClass())) {
            valueToValidate = ((Enum) valueToValidate).name();
        }
        if (Enum.class.isAssignableFrom(referencedValue.getClass())) {
            referencedValue = ((Enum) referencedValue).name();
        }
        return valueToValidate.equals(referencedValue);
    }

    @Override
    public boolean isSupportedType(Class<?> clazz) {
        if (clazz.isPrimitive()) {
            clazz = TypeHelper.PRIMITIVE_TO_WRAPPER_TYPES.get(clazz);
        }
        //TODO allow LocalDateTime and util.Date?!
        return String.class == clazz // String is final
                || Boolean.class == clazz // Boolean is final
                || Enum.class.isAssignableFrom(clazz)
                || Number.class.isAssignableFrom(clazz)
                || LocalDate.class == clazz;
    }
    
    @Override
    public String serializeToJson() {
        String type = getType();
        String valuesJson = getValues() != null ? "," + asKey("values") + asArray(getValues()) : "";
        return asKey("type") + quoted(type) + valuesJson;
    }

}

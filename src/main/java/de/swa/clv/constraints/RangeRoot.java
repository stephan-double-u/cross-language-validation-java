package de.swa.clv.constraints;

import de.swa.clv.util.TypeHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

import static de.swa.clv.json.JsonUtil.*;

public abstract class RangeRoot extends ConstraintRoot {

    private static Logger log = LoggerFactory.getLogger(RangeRoot.class);

    public static long SAVE_INTEGER_MAX = (1L << 53) -1;
    public static long SAVE_INTEGER_MIN = -((1L << 53) - 1);

    List<Object> maxValues;

    public List<Object> getMaxValues() {
        return maxValues;
    }

    @Override
    public boolean isSupportedType(final Class<?> clazz) {
        final Class<?> wrappedClass = (clazz.isPrimitive()) ? TypeHelper.PRIMITIVE_TO_WRAPPER_TYPES.get(clazz) : clazz;
        boolean classIsComarableNumber = Number.class.isAssignableFrom(wrappedClass) && Comparable.class.isAssignableFrom(wrappedClass);
        boolean classAndValuesHaveSameType = true;
        if (getValues() != null) {
            classAndValuesHaveSameType &= getValues().stream().allMatch(value -> value.getClass().equals(wrappedClass));
        }
        if (getMaxValues() != null) {
            classAndValuesHaveSameType &= getMaxValues().stream().allMatch(value -> value.getClass().equals(wrappedClass));
        }
        return (classIsComarableNumber || String.class == clazz) // String is final
                && classAndValuesHaveSameType;
    }

    @Override
    public String serializeToJson() {
        String minJson = getValues() != null ? "," + asKey("minValues") + asArray(getValues())
                : "";
        String maxJson = getMaxValues() != null ? "," + asKey("maxValues") + asArray(getMaxValues()) : "";
        return asKey("type") + quoted(getType()) + minJson + maxJson;
    }

}

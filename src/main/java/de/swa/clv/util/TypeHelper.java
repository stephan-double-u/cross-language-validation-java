package de.swa.clv.util;

import java.util.Map;

public abstract class TypeHelper {
    
    public static final Map<Class<?>, Class<?>> PRIMITIVE_TO_WRAPPER_TYPES = Map.of(
            boolean.class, Boolean.class,
            char.class, Character.class,
            byte.class, Byte.class,
            short.class, Short.class,
            int.class, Integer.class,
            long.class, Long.class,
            float.class, Float.class,
            double.class, Double.class);

    private TypeHelper() {
    }
}

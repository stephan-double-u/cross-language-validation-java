package de.swa.clv.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public abstract class TypeHelper {
    
    public static final Map<Class<?>, Class<?>> PRIMITIVE_TO_WRAPPER_TYPES;

    static {
        final Map<Class<?>, Class<?>> tmpMap = new HashMap<>(8);
        tmpMap.put( boolean.class, Boolean.class );
        tmpMap.put( char.class, Character.class );
        tmpMap.put( byte.class, Byte.class );
        tmpMap.put( short.class, Short.class );
        tmpMap.put( int.class, Integer.class );
        tmpMap.put( long.class, Long.class );
        tmpMap.put( float.class, Float.class );
        tmpMap.put( double.class, Double.class );
        PRIMITIVE_TO_WRAPPER_TYPES = Collections.unmodifiableMap(tmpMap);
    }

    private TypeHelper() {
    }
}

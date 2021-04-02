package de.swa.clv.json;

import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

public abstract class JsonUtil {

    private JsonUtil() {
    }

    public static String asObject(final String value) {
        return "{" + value + "}";
        
    }
    
    public static String asArray(final String value) {
        return "[" + value + "]";
        
    }
    
    public static String asKey(final String key) {
        return quoted(key) + ":";
        
    }

    public static String quoted(final String value) {
        return "\"" + value + "\"";
    }

    /**
     * Values of type String, Enumand LocalDate get quoted.
     *
     * @param objects TODO
     * @return TODO
     */
    public static String asArray(final List<Object> objects) {
        return asArray(objects.stream().map(JsonUtil::quoteIfNeeded).collect(Collectors.joining(",")));
    }

    private static String quoteIfNeeded(Object o) {
        return (o instanceof String || o instanceof Enum<?> || o instanceof LocalDate)
                ? quoted(o.toString()) : "" + o;
    }

}

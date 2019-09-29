package de.swa.easyvalidation.json;

import de.swa.easyvalidation.ValidationConditions;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public abstract class JsonUtil {

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
     * Values of type String and Enum get quoted
     * @param objects
     * @return
     */
    public static String asArray(final List<Object> objects) {
        String json = "";
        boolean first = true;
        for (final Object value : objects) {
            json += (!first ? "," : "");
            if (value != null && value instanceof String || value instanceof Enum<?> || value instanceof LocalDate) {
                json += quoted(value.toString());
            } else {
                json += value;
            }
            first = false;
        }
        return asArray(json);
    }

}

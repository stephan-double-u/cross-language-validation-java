package de.swa.easyvalidation.json;

import static de.swa.easyvalidation.json.JsonUtil.quoted;

import java.util.List;

import de.swa.easyvalidation.constraints.ConstraintRef;

public abstract class JsonUtil {

    public static String asObject(String value) {
        return "{" + value + "}";
        
    }
    
    public static String asArray(String value) {
        return "[" + value + "]";
        
    }
    
    public static String asKey(String key) {
        return quoted(key) + ":";
        
    }

    public static String quoted(String value) {
        return "\"" + value + "\"";
    }

    /**
     * Values of type String and Enum get quoted ...
     * @param objects
     * @return
     */
    public static String asArray(List<Object> objects) {
        String json = "";
        boolean first = true;
        for (Object value : objects) {
            json += (!first ? "," : "");
            if (value != null && value instanceof String || value instanceof Enum<?> ) {
                json += quoted(value.toString());
            } else {
                json += value;
            }
            first = false;
        }
        return asArray(json);
    }

}

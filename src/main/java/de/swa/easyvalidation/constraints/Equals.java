package de.swa.easyvalidation.constraints;
import static de.swa.easyvalidation.json.JsonUtil.asArray;
import static de.swa.easyvalidation.json.JsonUtil.asKey;
import static de.swa.easyvalidation.json.JsonUtil.asObject;
import static de.swa.easyvalidation.json.JsonUtil.quoted;

import java.sql.JDBCType;
import java.util.Arrays;
import java.util.Objects;

import de.swa.easyvalidation.util.TypeHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class Equals extends Constraint {

    abstract String getType();

    private static Logger log = LoggerFactory.getLogger(Equals.class);

    /*
     * Methods that create EqualsAny constraints
     */
    
    public static EqualsAny any(String... values) {
        return new EqualsAny(values);
    }
    
    public static EqualsAny any(Enum<?>... values) {
        return new EqualsAny(values);
    }
    
    public static EqualsAny any(Number... values) {
        return new EqualsAny(values);
    }
    
    // convenience methods ...
    public static EqualsAny true​_() {
        return new EqualsAny(true);
    }

    public static EqualsAny false​_() {
        return new EqualsAny(false);
    }

    public static EqualsAny null​_() {
        return new EqualsAny((Object) null);
    }
    
    /*
     * Methods that create EqualsNone constraints
     */
    
    public static EqualsNone none(String... values) {
        return new EqualsNone(values);
    }
    
    public static EqualsNone none(Enum<?>... values) {
        return new EqualsNone(values);
    }
    
    public static EqualsNone none(Number... values) {
        return new EqualsNone(values);
    }
    
    public static EqualsNone notNull() {
        return new EqualsNone((Object) null);
    }

    /*
     * Methods that create Equals*Ref constraints
     */
    
    public static EqualsAnyRef anyRef(String... propertyNames) {
        return new EqualsAnyRef(propertyNames);
    }
    
    public static EqualsNoneRef noneRef(String... propertyNames) {
        return new EqualsNoneRef(propertyNames);
    }
    
    
    @Override
    public boolean isSupportedType(Class<?> clazz) {
        if (clazz.isPrimitive()) {
            clazz = TypeHelper.PRIMITIVE_TO_WRAPPER_TYPES.get(clazz);
        }
        return String.class == clazz // is == o.k. here? String is final
                || Enum.class.isAssignableFrom(clazz)
                || Number.class.isAssignableFrom(clazz) 
                || Boolean.class == clazz; // Boolean is final
    }
    
    @Override
    public String serializeToJson() {
        String type = getType();
        return asObject(asKey("type") + quoted(type) + "," + asKey("values") + asArray(getValues()));
    }

}

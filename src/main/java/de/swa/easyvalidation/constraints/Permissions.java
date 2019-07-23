package de.swa.easyvalidation.constraints;

import static de.swa.easyvalidation.json.JsonUtil.asArray;
import static de.swa.easyvalidation.json.JsonUtil.asKey;
import static de.swa.easyvalidation.json.JsonUtil.asObject;
import static de.swa.easyvalidation.json.JsonUtil.quoted;

public abstract class Permissions extends ConstraintRoot {

    abstract String getType();

    public static PermissionsAny any(final String... values) {
        return new PermissionsAny(values);
    }
    
    public static PermissionsAny any(final Enum<?>... values) {
        return new PermissionsAny(values);
    }
    
    
    @Override
    public boolean isSupportedType(final Class<?> clazz) {
        return String.class == clazz
               || Enum.class.isAssignableFrom(clazz);
    }
    
    @Override
    public String serializeToJson() {
        final String type = getType();
        return asObject(asKey("type") + quoted(type) + "," + asKey("values") + asArray(getValues()));
    }

}

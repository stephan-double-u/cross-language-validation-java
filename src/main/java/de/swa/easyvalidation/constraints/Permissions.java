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
    public boolean validateValuesOrFail(final Class<?> ignore) {
        if (getValues().contains(null)) {
            throw new IllegalArgumentException("Permission values must not be null");
        }
        return true;
    }

    @Override
    public String serializeToJson() {
        if (getValues().isEmpty()) {
            return "";
        }
        return asKey("permissions") + asObject(asKey("type") + quoted(getType()) + "," + asKey("values") + asArray(getValues()));
    }

}

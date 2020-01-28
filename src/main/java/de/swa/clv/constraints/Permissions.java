package de.swa.clv.constraints;

import java.util.Arrays;

import static de.swa.clv.json.JsonUtil.asArray;
import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.asObject;
import static de.swa.clv.json.JsonUtil.quoted;

public abstract class Permissions extends ConstraintRoot {

    Permissions() {
    }

    /**
     * The related condition is validated only if at least one permission applies.
     *
     * @param values the permissions, {@code null} values are not allowed
     * @return a {@code PermissionsAny} object that holds the permissions
     */
    public static PermissionsAny any(final String... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new PermissionsAny(values);
    }

    /**
     * The related condition is validated only if at least one permission applies.
     * Important: Enum permissions are compared by names!
     *
     * @param values the permissions, {@code null} values are not allowed
     * @return a {@code PermissionsAny} object that holds the permissions
     */
    public static PermissionsAny any(final Enum<?>... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new PermissionsAny(values);
    }

    @Override
    public boolean isSupportedType(final Class<?> clazz) {
        return String.class == clazz
                || Enum.class.isAssignableFrom(clazz);
    }

    @Override
    public String serializeToJson() {
        if (getValues().isEmpty()) {
            return "";
        }
        return asKey("permissions") + asObject(asKey("type") + quoted(getType()) + "," + asKey("values") + asArray(getValues()));
    }

}

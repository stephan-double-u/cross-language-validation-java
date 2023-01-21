package de.swa.clv.constraints;

import java.util.Arrays;
import java.util.List;

import static de.swa.clv.json.JsonUtil.asArray;
import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.asObject;
import static de.swa.clv.json.JsonUtil.quoted;

public abstract class Permissions extends ConstraintRoot {

    Permissions() {
    }

    public abstract boolean validate(final List<Object> userPermissions);

    /**
     * The related condition is validated only if at least one permission applies.
     *
     * @param values the permissions, {@code null} values are not allowed
     * @return a {@code PermissionsAny} object that holds the permissions
     */
    public static PermissionsAny any(String... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException(NULL_VALUE_ERR_MESSAGE);
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
    public static PermissionsAny any(Enum<?>... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException(NULL_VALUE_ERR_MESSAGE);
        }
        String[] valuesAsStrings = Arrays.stream(values).map(Enum::name).toArray(String[]::new);
        return new PermissionsAny(valuesAsStrings);
    }

    /**
     * The related condition is validated only if all permissions applies.
     *
     * @param values the permissions, {@code null} values are not allowed
     * @return a {@code PermissionsAny} object that holds the permissions
     */
    public static PermissionsAll all(String... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException(NULL_VALUE_ERR_MESSAGE);
        }
        return new PermissionsAll(values);
    }

    /**
     * The related condition is validated only if all permissions applies.
     * Important: Enum permissions are compared by names!
     *
     * @param values the permissions, {@code null} values are not allowed
     * @return a {@code PermissionsAny} object that holds the permissions
     */
    public static PermissionsAll all(Enum<?>... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException(NULL_VALUE_ERR_MESSAGE);
        }
        String[] valuesAsStrings = Arrays.stream(values).map(Enum::name).toArray(String[]::new);
        return new PermissionsAll(valuesAsStrings);
    }

    /**
     * The related condition is validated only if none permission applies.
     *
     * @param values the permissions, {@code null} values are not allowed
     * @return a {@code PermissionsAny} object that holds the permissions
     */
    public static PermissionsNone none(String... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException(NULL_VALUE_ERR_MESSAGE);
        }
        return new PermissionsNone(values);
    }

    /**
     * The related condition is validated only if none permission applies.
     * Important: Enum permissions are compared by names!
     *
     * @param values the permissions, {@code null} values are not allowed
     * @return a {@code PermissionsAny} object that holds the permissions
     */
    public static PermissionsNone none(Enum<?>... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException(NULL_VALUE_ERR_MESSAGE);
        }
        String[] valuesAsStrings = Arrays.stream(values).map(Enum::name).toArray(String[]::new);
        return new PermissionsNone(valuesAsStrings);
    }

    @Override
    public boolean isSupportedType(Class<?> clazz) {
        return String.class == clazz
                || Enum.class.isAssignableFrom(clazz);
    }

    @Override
    public String serializeToJson() {
        if (getValues().isEmpty()) {
            return "";
        }
        return asKey("permissions") + asObject(asKey("type") + quoted(getToken()) + "," + asKey("values") + asArray(getValues()));
    }

}

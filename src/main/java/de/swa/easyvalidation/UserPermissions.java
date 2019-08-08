package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.*;

import java.util.List;

public class UserPermissions {

    private final List<Object> values;

    public UserPermissions(final String... values) {
        this.values = Permissions.any(values).getValues();
    }

    public <E extends Enum<?>> UserPermissions(final E... values) {
        this.values = Permissions.any(values).getValues();
    }

    public static UserPermissions of(final String... values) {
        return new UserPermissions(values);
    }

    public static UserPermissions of(final Enum<?>... values) {
        return new UserPermissions(values);
    }

    public List<Object> getValues() {
        return values;
    }

}

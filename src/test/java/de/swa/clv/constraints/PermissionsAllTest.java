package de.swa.clv.constraints;

import de.swa.clv.UserPermissions;
import org.junit.jupiter.api.Test;

import static de.swa.clv.constraints.Constraint.NULL_VALUE_ERR_MESSAGE;
import static org.junit.jupiter.api.Assertions.*;

class PermissionsAllTest {

    @Test
    void exceptionIfStringIsNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Permissions.all("ONE", null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void exceptionIfEnumIsNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Permissions.all(SomeEnum.ONE, null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void validateEnumsVsStrings_true() {
        PermissionsAll all = Permissions.all(SomeEnum.ONE, OtherEnum.FOO);
        UserPermissions userPermissions = UserPermissions.of("FOO", "ONE", "TWO");
        assertTrue(all.validate(userPermissions.getValues()));
    }

    @Test
    void validateEnumsVsStrings_false() {
        PermissionsAll all = Permissions.all(SomeEnum.ONE, OtherEnum.FOO);
        UserPermissions userPermissions = UserPermissions.of("FOO");
        assertFalse(all.validate(userPermissions.getValues()));
    }

    @Test
    void validateStringsVsEnums_true() {
        PermissionsAll all = Permissions.all("FOO", "ONE");
        UserPermissions userPermissions = UserPermissions.of(SomeEnum.ONE, OtherEnum.FOO, OtherEnum.BAR);
        assertTrue(all.validate(userPermissions.getValues()));
    }

    @Test
    void validateStringsVsEnums_false() {
        PermissionsAll all = Permissions.all( "FOO", "BAR");
        UserPermissions userPermissions = UserPermissions.of(OtherEnum.BAR);
        assertFalse(all.validate(userPermissions.getValues()));
    }

    enum SomeEnum {
        ONE, TWO
    }

    enum OtherEnum {
        FOO, BAR
    }
}

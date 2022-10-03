package de.swa.clv.constraints;

import de.swa.clv.UserPermissions;
import org.junit.jupiter.api.Test;

import static de.swa.clv.constraints.ConstraintRoot.NULL_VALUE_ERR_MESSAGE;
import static org.junit.jupiter.api.Assertions.*;

public class PermissionsAnyTest {

    @Test
    public void exceptionIfStringIsNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Permissions.any("ONE", null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    public void exceptionIfEnumIsNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Permissions.any(SomeEnum.ONE, null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    public void validateEnumsVsStrings_true() {
        PermissionsAny all = Permissions.any(SomeEnum.ONE, OtherEnum.FOO);
        UserPermissions userPermissions = UserPermissions.of("TWO", "FOO");
        assertTrue(all.validate(userPermissions.getValues()));
    }

    @Test
    public void validateEnumsVsStrings_false() {
        PermissionsAny all = Permissions.any(SomeEnum.ONE, OtherEnum.FOO);
        UserPermissions userPermissions = UserPermissions.of("TWO", "BAR");
        assertFalse(all.validate(userPermissions.getValues()));
    }

    @Test
    public void validateStringsVsEnums_true() {
        PermissionsAny all = Permissions.any("FOO", "ONE");
        UserPermissions userPermissions = UserPermissions.of(OtherEnum.BAR, OtherEnum.FOO);
        assertTrue(all.validate(userPermissions.getValues()));
    }

    @Test
    public void validateStringsVsEnums_false() {
        PermissionsAny all = Permissions.any( "FOO", "ONE");
        UserPermissions userPermissions = UserPermissions.of(SomeEnum.TWO);
        assertFalse(all.validate(userPermissions.getValues()));
    }

    enum SomeEnum {
        ONE, TWO
    }

    enum OtherEnum {
        FOO, BAR
    }
}

package de.swa.clv.constraints;

import de.swa.clv.UserPermissions;
import org.junit.jupiter.api.Test;

import static de.swa.clv.constraints.Constraint.NULL_VALUE_ERR_MESSAGE;
import static org.junit.jupiter.api.Assertions.*;

class PermissionsNoneTest {

    @Test
    void exceptionIfStringIsNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, 
                () -> Permissions.none("ONE", null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void exceptionIfEnumIsNull() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Permissions.none(SomeEnum.ONE, null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void validateEnumsVsStrings_true() {
        PermissionsNone none = Permissions.none(SomeEnum.ONE, OtherEnum.FOO);
        UserPermissions userPermissions = UserPermissions.of("BAR", "TWO");
        assertTrue(none.validate(userPermissions.getValues()));
    }

    @Test
    void validateEnumsVsStrings_false() {
        PermissionsNone none = Permissions.none(SomeEnum.ONE, OtherEnum.FOO);
        UserPermissions userPermissions = UserPermissions.of("FOO");
        assertFalse(none.validate(userPermissions.getValues()));
    }

    @Test
    void validateStringsVsEnums_true() {
        PermissionsNone none = Permissions.none("FOO", "ONE");
        UserPermissions userPermissions = UserPermissions.of(SomeEnum.TWO, OtherEnum.BAR);
        assertTrue(none.validate(userPermissions.getValues()));
    }

    @Test
    void validateStringsVsEnums_false() {
        PermissionsNone none = Permissions.none( "FOO", "BAR");
        UserPermissions userPermissions = UserPermissions.of(OtherEnum.BAR);
        assertFalse(none.validate(userPermissions.getValues()));
    }

    enum SomeEnum {
        ONE, TWO
    }

    enum OtherEnum {
        FOO, BAR
    }
}

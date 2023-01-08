package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

class ValueUnchangedTest {

    ValueUnchanged constraint = Value.unchanged();

    @Test
    void isSupportedType_Serializable() {
        assertTrue(constraint.isSupportedType(UUID.class));
    }

    @Test
    void validateValueUnchangedTrue() {
        UUID thisUuid = UUID.randomUUID();
        UUID thatUuid = UUID.fromString(thisUuid.toString());
        assertTrue(constraint.validate(thisUuid, thatUuid));
    }

    @Test
    void validateValueUnchangedFalse() {
        UUID thisUuid = UUID.randomUUID();
        UUID thatUuid = UUID.randomUUID();
        assertFalse(constraint.validate(thisUuid, thatUuid));
    }

    @Test
    void serializeToJson() {
        assertEquals("""
                "type":"VALUE_UNCHANGED\"""", constraint.serializeToJson());
    }
}

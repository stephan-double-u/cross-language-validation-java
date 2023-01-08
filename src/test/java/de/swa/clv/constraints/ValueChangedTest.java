package de.swa.clv.constraints;

import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

class ValueChangedTest {

    ValueChanged constraint = Value.changed();

    @Test
     void isSupportedType_Serializable() {
        assertTrue(constraint.isSupportedType(UUID.class));
    }

    @Test
    void validateValueChangedTrue() {
        UUID thisUuid = UUID.randomUUID();
        UUID thatUuid = UUID.randomUUID();
        assertTrue(constraint.validate(thisUuid, thatUuid));
    }

    @Test
    void validateValueChangedFasle() {
        UUID thisUuid = UUID.randomUUID();
        UUID thatUuid = UUID.fromString(thisUuid.toString());
        assertFalse(constraint.validate(thisUuid, thatUuid));
    }

    @Test
    void serializeToJson() {
        assertEquals("""
                "type":"VALUE_CHANGED\"""", constraint.serializeToJson());
    }
}

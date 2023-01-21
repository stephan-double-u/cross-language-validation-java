package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class EqualsNotNullTest {

    EqualsNotNull notNull = Equals.notNull();

    @Test
    void isSupportedType_Serializable() {
        assertTrue(notNull.isSupportedType(UUID.class));
    }

    @Test
    void validateUuidIsNotNull() {
        UUID uiid = UUID.randomUUID();
        assertTrue(notNull.validate(uiid, null));
    }
    @Test
    void serializeToJson() {
        assertEquals(Util.doubleQuote("'type':'EQUALS_NOT_NULL'"), notNull.serializeToJson());
    }

}

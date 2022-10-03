package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class EqualsNotNullTest {

    EqualsNotNull notNull = Equals.notNull();

    @Test
    public void isSupportedType_Serializable() {
        assertTrue(notNull.isSupportedType(UUID.class));
    }

    @Test
    public void validateUuidIsNotNull() {
        UUID uiid = UUID.randomUUID();
        assertTrue(notNull.validate(uiid, null));
    }
    @Test
    public void serializeToJson() {
        assertEquals(Util.doubleQuote("'type':'EQUALS_NOT_NULL'"), notNull.serializeToJson());
    }

}

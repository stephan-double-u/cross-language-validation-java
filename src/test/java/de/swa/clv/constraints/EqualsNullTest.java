package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class EqualsNullTest {

    EqualsNull null_ = Equals.null_();

    @Test
    void isSupportedType_Serializable() {
        assertTrue(null_.isSupportedType(UUID.class));
    }

    @Test
    void validateUuidIsNull() {
        UUID uiid = null;
        assertTrue(null_.validate(uiid, null));
    }

    @Test
    void serializeToJson() {
        assertEquals(Util.doubleQuote("'type':'EQUALS_NULL'"), null_.serializeToJson());
    }

}

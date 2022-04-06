package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.Test;

import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class EqualsNullTest {

    EqualsNull null_ = Equals.null_();

    @Test
    public void isSupportedType_Serializable() {
        assertTrue(null_.isSupportedType(UUID.class));
    }

    @Test
    public void validateUuidIsNull() {
        UUID uiid = null;
        assertTrue(null_.validate(uiid, null));
    }

    @Test
    public void serializeToJson() {
        assertEquals(Util.doubleQuote("'type':'EQUALS_NULL'"), null_.serializeToJson());
    }

}

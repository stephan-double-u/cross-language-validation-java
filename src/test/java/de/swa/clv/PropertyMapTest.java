package de.swa.clv;

import de.swa.clv.test.Util;
import org.junit.Test;

import static org.junit.Assert
.assertEquals;

public class PropertyMapTest {

    @Test
    public void serializeEmptyMap() {
        PropertyMap pm = new PropertyMap();
        assertEquals("", pm.serializeToJson());
    }

    @Test
    public void serializeNonEmptyMap() {
        PropertyMap pm = new PropertyMap();
        pm.getOrInit("foo");
        pm.getOrInit("bar");
        assertEquals(Util.doubleQuote("'foo':[],'bar':[]"), pm.serializeToJson());
    }
}

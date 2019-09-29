package de.swa.easyvalidation;

import de.swa.easyvalidation.test.Util;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

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
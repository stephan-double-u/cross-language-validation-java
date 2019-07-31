package de.swa.easyvalidation;

import org.junit.Test;

public class PropertyMapTest {

    @Test
    public void serializeToJson() {
        PropertyMap pm = new PropertyMap();
        System.out.println(pm.serializeToJson());
    }

    @Test
    public void serializeToJson2() {
        PropertyMap pm = new PropertyMap();
        pm.getValueOrInit("foo");
        pm.getValueOrInit("bar");
        System.out.println(pm.serializeToJson());
    }
}
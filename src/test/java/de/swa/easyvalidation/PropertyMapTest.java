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
        pm.getOrInit("foo");
        pm.getOrInit("bar");
        System.out.println(pm.serializeToJson());
    }
}
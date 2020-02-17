package de.swa.clv.groups;

import de.swa.clv.test.Util;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class RelationsSubGroupTest {

    @Test
    public void serializeAndGroup() {
        String json = new RelationsAndGroup().serializeToJson();
        assertEquals(Util.doubleQuote("{'operator':'AND','constraints':[]}"), json);
    }

    @Test
    public void serializeOrGroup() {
        String json = new RelationsOrGroup().serializeToJson();
        assertEquals(Util.doubleQuote("{'operator':'OR','constraints':[]}"), json);
    }

}
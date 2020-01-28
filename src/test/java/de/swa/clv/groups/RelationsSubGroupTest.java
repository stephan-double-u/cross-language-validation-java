package de.swa.clv.groups;

import de.swa.clv.test.Util;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class RelationsSubGroupTest {

    @Test
    public void serializeAndGroup() {
        String json = new AndGroup().serializeToJson();
        assertEquals(Util.doubleQuote("{'operator':'AND','constraints':[]}"), json);
    }

    @Test
    public void serializeOrGroup() {
        String json = new OrGroup().serializeToJson();
        assertEquals(Util.doubleQuote("{'operator':'OR','constraints':[]}"), json);
    }

}
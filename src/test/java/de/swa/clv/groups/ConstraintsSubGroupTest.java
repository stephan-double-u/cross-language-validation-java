package de.swa.clv.groups;

import de.swa.clv.test.Util;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ConstraintsSubGroupTest {

    @Test
    public void serializeAndGroup() {
        String json = new ConstraintsAndGroup().serializeToJson();
        assertEquals(Util.doubleQuote("{'operator':'AND','constraints':[]}"), json);
    }

    @Test
    public void serializeOrGroup() {
        String json = new ConstraintsOrGroup().serializeToJson();
        assertEquals(Util.doubleQuote("{'operator':'OR','constraints':[]}"), json);
    }

}
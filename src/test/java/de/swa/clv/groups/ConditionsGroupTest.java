package de.swa.clv.groups;

import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ConditionsGroupTest {

    @Test
    void serializeAndGroup() {
        String json = new ConditionsAndGroup().serializeToJson();
        assertEquals(Util.doubleQuote("{'operator':'AND','conditions':[]}"), json);
    }

    @Test
    void serializeOrGroup() {
        String json = new ConditionsOrGroup().serializeToJson();
        assertEquals(Util.doubleQuote("{'operator':'OR','conditions':[]}"), json);
    }

}

package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.constraints.Size;
import de.swa.easyvalidation.test.Util;
import org.junit.Test;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;

public class ConstraintsSubGroupTest {

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
package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.test.Util;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ConstraintRefTest {

    @Test
    public void serializeToJson() {
        final ConstraintRef constraintRef = new ConstraintRef("prop1", Size.minMax(1, 100));
        assertEquals(Util.doubleQuote("{'property':'prop1','type':'SIZE','min':1,'max':100}"), constraintRef.serializeToJson());
    }
}

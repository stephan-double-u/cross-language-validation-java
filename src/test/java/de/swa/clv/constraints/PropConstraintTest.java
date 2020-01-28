package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class PropConstraintTest {

    @Test
    public void serializeToJson() {
        final PropConstraint propConstraint = new PropConstraint("prop1", Size.minMax(1, 100));
        assertEquals(Util.doubleQuote("{'property':'prop1','type':'SIZE','min':1,'max':100}"), propConstraint.serializeToJson());
    }
}

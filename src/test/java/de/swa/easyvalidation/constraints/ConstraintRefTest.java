package de.swa.easyvalidation.constraints;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ConstraintRefTest {

    // TODO more and better tests ...

    @Test
    public void serializeToJson() {
        final ConstraintRef constraintRef = new ConstraintRef("prop1", Size.minMax(1, 100));
        assertEquals("{\"property\":\"prop1\",\"type\":\"SIZE\",\"min\":1,\"max\":100}", constraintRef.serializeToJson());
    }
}

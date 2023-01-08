package de.swa.clv.constraints;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ConditionConstraintTest {

    @Test
    public void serializeToJson() {
        final ConditionConstraint conditionConstraint = new ConditionConstraint("prop1", Size.minMax(1, 100));
        assertEquals("""
                {"property":"prop1","constraint":{"type":"SIZE","min":1,"max":100}}""",
                conditionConstraint.serializeToJson());
    }
}

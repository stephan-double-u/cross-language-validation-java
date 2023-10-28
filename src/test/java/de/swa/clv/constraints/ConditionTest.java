package de.swa.clv.constraints;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ConditionTest {

    @Test
    void serializeToJson() {
        final Condition condition = Condition.of("prop1", Size.minMax(1, 100));
        assertEquals("""
                {"property":"prop1","constraint":{"type":"SIZE","min":1,"max":100}}""",
                condition.serializeToJson());
    }
}

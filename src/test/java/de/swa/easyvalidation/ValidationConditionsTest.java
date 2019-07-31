package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.constraints.ConstraintRef;
import de.swa.easyvalidation.constraints.Permissions;
import de.swa.easyvalidation.constraints.Size;
import org.hamcrest.core.StringContains;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.*;

public class ValidationConditionsTest {
    final static ValidationConditions<ObjectUnderTest> conditions = new ValidationConditions<>(ObjectUnderTest.class);
    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Test
    public void mandatoryPropertyNotUnique() {
        conditions.mandatory("prop1");

        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Validation conditions for property 'prop1'"));

        conditions.mandatory("prop1", Constraint.ref("prop1", Size.minMax(1, 100)));
    }

    @Test
    public void serializeToJson() {
        assertEquals("'objectundertest':{'mandatoryConditions':{},'immutableConditions':{},'contentConditions':{}}",
                conditions.serializeToJson().replace("\"", "'"));
    }

    @Test
    public void manyManyMore() {
        assertTrue(true);
    }

    class ObjectUnderTest {
        private String prop1;

        public String getProp1() {
            return prop1;
        }

    }
}
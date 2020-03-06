package de.swa.clv.groups;

import de.swa.clv.constraints.Constraint;
import de.swa.clv.constraints.Equals;
import de.swa.clv.test.Util;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ConstraintsTopGroupTest {

    @Test
    public void serializeEmpty() {
        ConstraintsTopGroup topGroup = ConstraintsTopGroup.AND();
        assertEquals("",topGroup.serializeToJson());
    }
    @Test
    public void serializeToJson() {
        ConstraintsTopGroup topGroup = ConstraintsTopGroup.AND(
                ConstraintsSubGroup.OR(
                        Constraint.ref("prop1", Equals.any("FOO")),
                        Constraint.ref("prop2", Equals.any("BAR"))),
                ConstraintsSubGroup.AND(
                        Constraint.ref("prop3", Equals.any("ZOO")))
        );
        final String expected = Util.doubleQuote("'constraintsTopGroup':{'operator':'AND','constraintsSubGroups':[" +
                "{'operator':'OR','constraints':[{'property':'prop1','type':'EQUALS_ANY','values':['FOO']},{'property':'prop2','type':'EQUALS_ANY','values':['BAR']}]}," +
                "{'operator':'AND','constraints':[{'property':'prop3','type':'EQUALS_ANY','values':['ZOO']}]}]}");
        assertEquals(expected,topGroup.serializeToJson());
    }

}
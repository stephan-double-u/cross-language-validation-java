package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.constraints.Equals;
import de.swa.easyvalidation.test.Util;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ConstraintsTopGroupTest {

    @Test
    public void serializeEmpty() {
        ConstraintsTopGroup topGroup = ConstraintsTopGroup.anded();
        assertEquals("",topGroup.serializeToJson());
    }
    @Test
    public void serializeToJson() {
        ConstraintsTopGroup topGroup = ConstraintsTopGroup.anded(
                ConstraintsSubGroup.or(
                        Constraint.ref("prop1", Equals.any("FOO")),
                        Constraint.ref("prop2", Equals.any("BAR"))),
                ConstraintsSubGroup.and(
                        Constraint.ref("prop3", Equals.any("ZOO")))
        );
        final String expected = Util.doubleQuote("'constraintsTopGroup':{'operator':'AND','constraintsSubGroups':[" +
                "{'operator':'OR','constraints':[{'property':'prop1','type':'EQUALS_ANY','values':['FOO']},{'property':'prop2','type':'EQUALS_ANY','values':['BAR']}]}," +
                "{'operator':'AND','constraints':[{'property':'prop3','type':'EQUALS_ANY','values':['ZOO']}]}]}");
        assertEquals(expected,topGroup.serializeToJson());
    }

}
package de.swa.clv.groups;

import de.swa.clv.constraints.Constraint;
import de.swa.clv.constraints.Equals;
import de.swa.clv.test.Util;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class RelationsTopGroupTest {

    @Test
    public void serializeEmpty() {
        RelationsTopGroup topGroup = RelationsTopGroup.AND();
        assertEquals("",topGroup.serializeToJson());
    }
    @Test
    public void serializeToJson() {
        RelationsTopGroup topGroup = RelationsTopGroup.AND(
                RelationsSubGroup.OR(
                        Constraint.ref("prop1", Equals.any("FOO")),
                        Constraint.ref("prop2", Equals.any("BAR"))),
                RelationsSubGroup.AND(
                        Constraint.ref("prop3", Equals.any("ZOO")))
        );
        final String expected = Util.doubleQuote("'relationsTopGroup':{'operator':'AND','relationsSubGroups':[" +
                "{'operator':'OR','constraints':[{'property':'prop1','type':'EQUALS_ANY','values':['FOO']},{'property':'prop2','type':'EQUALS_ANY','values':['BAR']}]}," +
                "{'operator':'AND','constraints':[{'property':'prop3','type':'EQUALS_ANY','values':['ZOO']}]}]}");
        assertEquals(expected,topGroup.serializeToJson());
    }

}
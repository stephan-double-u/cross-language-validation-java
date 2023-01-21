package de.swa.clv.groups;

import de.swa.clv.constraints.Condition;
import de.swa.clv.constraints.Equals;
import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ConditionsTopGroupTest {

    @Test
    void serializeEmpty() {
        ConditionsTopGroup topGroup = ConditionsTopGroup.AND();
        assertEquals("",topGroup.serializeToJson());
    }

    @Test
    void serializeToJson_singleCondition() {
        ConditionsTopGroup topGroup = ConditionsTopGroup.AND(
                ConditionsGroup.OR(
                        Condition.of("prop2", Equals.any("BAR")))
        );
        final String expected = Util.doubleQuote("'condition':{" +
                "'property':'prop2','constraint':{'type':'EQUALS_ANY','values':['BAR']}" +
                "}");
        assertEquals(expected, topGroup.serializeToJson());
    }

    @Test
    void serializeToJson_singleSubGroup() {
        ConditionsTopGroup topGroup = ConditionsTopGroup.AND(
                ConditionsGroup.OR(
                        Condition.of("prop1", Equals.any("FOO")),
                        Condition.of("prop2", Equals.any("BAR")))
        );
        final String expected = Util.doubleQuote("'conditionsGroup':" +
                "{'operator':'OR','conditions':[" +
                "{'property':'prop1','constraint':{'type':'EQUALS_ANY','values':['FOO']}}," +
                "{'property':'prop2','constraint':{'type':'EQUALS_ANY','values':['BAR']}}" +
                "]}");
        assertEquals(expected, topGroup.serializeToJson());
    }

    @Test
    void serializeToJson_topGroup() {
        ConditionsTopGroup topGroup = ConditionsTopGroup.AND(
                ConditionsGroup.OR(
                        Condition.of("prop1", Equals.any("FOO")),
                        Condition.of("prop2", Equals.any("BAR"))),
                ConditionsGroup.AND(
                        Condition.of("prop3", Equals.any("ZOO")))
        );
        final String expected = Util.doubleQuote("'conditionsTopGroup':{'operator':'AND','conditionsGroups':[" +
                "{'operator':'OR','conditions':[" +
                "{'property':'prop1','constraint':{'type':'EQUALS_ANY','values':['FOO']}}," +
                "{'property':'prop2','constraint':{'type':'EQUALS_ANY','values':['BAR']}}" +
                "]}," +
                "{'operator':'AND','conditions':[" +
                "{'property':'prop3','constraint':{'type':'EQUALS_ANY','values':['ZOO']}}" +
                "]}" +
                "]}");
        assertEquals(expected, topGroup.serializeToJson());
    }

}

package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.constraints.Equals;
import org.junit.Test;

public class ConstraintsTopGroupTest {

    @Test
    public void serializeToJson() {
        ConstraintsTopGroup topGroup = ConstraintsTopGroup.anded(
                ConstraintsSubGroup.or(
                        Constraint.ref("prop1", Equals.any("FOO")),
                        Constraint.ref("prop2", Equals.any("BAR"))),
                ConstraintsSubGroup.and(
                        Constraint.ref("prop3", Equals.any("ZOO")))
        );
        System.out.println(topGroup.serializeToJson());
    }

    @Test
    public void serializeToJson2() {
        ConstraintsTopGroup topGroup = ConstraintsTopGroup.anded();
        System.out.println(topGroup.serializeToJson());
    }
}
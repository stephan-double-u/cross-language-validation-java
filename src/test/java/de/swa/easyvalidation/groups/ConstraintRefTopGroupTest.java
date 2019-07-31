package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.constraints.ConstraintRef;
import de.swa.easyvalidation.constraints.Equals;
import org.junit.Test;

import static org.junit.Assert.*;

public class ConstraintRefTopGroupTest {

    @Test
    public void serializeToJson() {
        ConstraintRefTopGroup topGroup = ConstraintRefTopGroup.anded(
                ConstraintRefGroup.or(
                        Constraint.ref("prop1", Equals.any("FOO")),
                        Constraint.ref("prop2", Equals.any("BAR"))),
                ConstraintRefGroup.and(
                        Constraint.ref("prop3", Equals.any("ZOO")))
        );
        System.out.println(topGroup.serializeToJson());
    }

    @Test
    public void serializeToJson2() {
        ConstraintRefTopGroup topGroup = ConstraintRefTopGroup.anded();
        System.out.println(topGroup.serializeToJson());
    }
}
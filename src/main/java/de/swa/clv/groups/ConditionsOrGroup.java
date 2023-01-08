package de.swa.clv.groups;

import de.swa.clv.constraints.ConditionConstraint;

public class ConditionsOrGroup extends ConditionsGroup {

    protected ConditionsOrGroup(final ConditionConstraint... oredConditionConstraints) {
        constraints = oredConditionConstraints;
    }

}

package de.swa.clv.groups;

import de.swa.clv.constraints.ConditionConstraint;

public class ConditionsAndGroup extends ConditionsGroup {

    protected ConditionsAndGroup(final ConditionConstraint... andedConditionConstraints) {
        constraints = andedConditionConstraints;
    }

}

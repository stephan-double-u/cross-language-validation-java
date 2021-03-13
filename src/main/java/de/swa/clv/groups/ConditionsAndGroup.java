package de.swa.clv.groups;

import de.swa.clv.constraints.PropConstraint;

public class ConditionsAndGroup extends ConditionsGroup {

    protected ConditionsAndGroup(final PropConstraint... andedPropConstraints) {
        propConstraints = andedPropConstraints;
    }

}

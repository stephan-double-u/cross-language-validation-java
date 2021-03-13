package de.swa.clv.groups;

import de.swa.clv.constraints.PropConstraint;

public class ConditionsOrGroup extends ConditionsGroup {

    protected ConditionsOrGroup(final PropConstraint... oredPropConstraints) {
        propConstraints = oredPropConstraints;
    }

}

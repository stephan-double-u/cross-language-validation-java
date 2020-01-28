package de.swa.clv.groups;

import de.swa.clv.constraints.PropConstraint;

public class OrGroup extends RelationsSubGroup {

    protected OrGroup(final PropConstraint... oredPropConstraints) {
        propConstraints = oredPropConstraints;
    }

}

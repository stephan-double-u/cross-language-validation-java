package de.swa.clv.groups;

import de.swa.clv.constraints.PropConstraint;

public class RelationsOrGroup extends RelationsSubGroup {

    protected RelationsOrGroup(final PropConstraint... oredPropConstraints) {
        propConstraints = oredPropConstraints;
    }

}

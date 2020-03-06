package de.swa.clv.groups;

import de.swa.clv.constraints.PropConstraint;

public class ConstraintsOrGroup extends ConstraintsSubGroup {

    protected ConstraintsOrGroup(final PropConstraint... oredPropConstraints) {
        propConstraints = oredPropConstraints;
    }

}

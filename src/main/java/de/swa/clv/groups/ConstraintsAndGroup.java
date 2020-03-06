package de.swa.clv.groups;

import de.swa.clv.constraints.PropConstraint;

public class ConstraintsAndGroup extends ConstraintsSubGroup {

    protected ConstraintsAndGroup(final PropConstraint... andedPropConstraints) {
        propConstraints = andedPropConstraints;
    }

}

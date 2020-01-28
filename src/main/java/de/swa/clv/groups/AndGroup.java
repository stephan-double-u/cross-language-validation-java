package de.swa.clv.groups;

import de.swa.clv.constraints.PropConstraint;

public class AndGroup extends RelationsSubGroup {

    protected AndGroup(final PropConstraint... andedPropConstraints) {
        propConstraints = andedPropConstraints;
    }

}

package de.swa.clv.groups;

import de.swa.clv.constraints.PropConstraint;

public class RelationsAndGroup extends RelationsSubGroup {

    protected RelationsAndGroup(final PropConstraint... andedPropConstraints) {
        propConstraints = andedPropConstraints;
    }

}

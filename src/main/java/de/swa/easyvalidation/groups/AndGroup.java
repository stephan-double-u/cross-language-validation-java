package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.PropConstraint;

public class AndGroup extends RelationsSubGroup {

    protected AndGroup(final PropConstraint... andedPropConstraints) {
        propConstraints = andedPropConstraints;
    }

}

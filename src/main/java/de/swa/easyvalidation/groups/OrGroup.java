package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.PropConstraint;

public class OrGroup extends RelationsSubGroup {

    protected OrGroup(final PropConstraint... oredPropConstraints) {
        propConstraints = oredPropConstraints;
    }

}

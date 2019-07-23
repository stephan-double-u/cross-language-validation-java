package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.ConstraintRef;

public class OrGroup extends ConstraintRefGroup {

    protected OrGroup(final ConstraintRef... oredConstraintRefs) {
        constraintRefs = oredConstraintRefs;
    }

}

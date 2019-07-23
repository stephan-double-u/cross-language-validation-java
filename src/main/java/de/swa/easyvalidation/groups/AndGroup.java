package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.ConstraintRef;

public class AndGroup extends ConstraintRefGroup {

    protected AndGroup(final ConstraintRef... andedConstraintRefs) {
        constraintRefs = andedConstraintRefs;
    }

}

package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.ConstraintRef;

public class AndGroup extends ConstraintsSubGroup {

    protected AndGroup(final ConstraintRef... andedConstraintRefs) {
        constraintRefs = andedConstraintRefs;
    }

}

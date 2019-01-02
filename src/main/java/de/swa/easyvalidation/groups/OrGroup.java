package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.ConstraintRef;

public class OrGroup extends ConstraintRefGroup  {

    protected OrGroup(ConstraintRef... oredConstraintRefs) {
        this.constraintRefs = oredConstraintRefs;
    }

}

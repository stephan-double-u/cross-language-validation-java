package de.swa.clv.groups;

import de.swa.clv.constraints.ConstraintRoot;

public class ContentConstraints {

    private final ConstraintRoot contentConstraint;
    private final ConstraintsTopGroup constraintsTopGroup;

    public ContentConstraints(final ConstraintRoot contentConstraint, final ConstraintsTopGroup constraintsTopGroup) {
        this.contentConstraint = contentConstraint;
        this.constraintsTopGroup = constraintsTopGroup;
    }

    public ConstraintRoot getContentConstraint() {
        return contentConstraint;
    }

    public ConstraintsTopGroup getConstraintsTopGroup() {
        return constraintsTopGroup;
    }

}

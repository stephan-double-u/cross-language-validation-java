package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.json.JsonSerializable;

public class ContentContraintGroup implements JsonSerializable {

    private final Constraint contentConstraint;
    private final ConstraintRefTopGroup constraintRefTopGroup;

    public ContentContraintGroup(final Constraint contentConstraint, final ConstraintRefTopGroup constraintRefTopGroup) {
        this.contentConstraint = contentConstraint;
        this.constraintRefTopGroup = constraintRefTopGroup;
    }

    public static ContentContraintGroup ored(final Constraint contentConstraint, final ConstraintRefTopGroup constraintRefTopGroup) {
        return new ContentContraintGroup(contentConstraint, constraintRefTopGroup);
    }

    public Constraint getContentConstraint() {
        return contentConstraint;
    }

    public ConstraintRefTopGroup getConstraintRefTopGroup() {
        return constraintRefTopGroup;
    }

    @Override
    public String serializeToJson() {
        // TODO Auto-generated method stub
        return "TODO";
    }

}

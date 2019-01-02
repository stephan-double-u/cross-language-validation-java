package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.constraints.ConstraintRef;
import de.swa.easyvalidation.json.JsonSerializable;

public class ContentGroup implements JsonSerializable {

    private Constraint contentConstraint;
    private ConstraintRefGroups constraintRefGroups;

    private ContentGroup(Constraint contentConstraint, ConstraintRefGroups constraintRefGroups) {
        this.contentConstraint = contentConstraint;
        this.constraintRefGroups = constraintRefGroups;
    }

    public static ContentGroup ored(Constraint contentConstraint, ConstraintRefGroups constraintRefGroups) {
        return new ContentGroup(contentConstraint, constraintRefGroups);
    }

    public Constraint getContentConstraint() {
        return contentConstraint;
    }

    public ConstraintRefGroups getConstraintRefGroups() {
        return constraintRefGroups;
    }

    @Override
    public String serializeToJson() {
        // TODO Auto-generated method stub
        return "TODO";
    }

}

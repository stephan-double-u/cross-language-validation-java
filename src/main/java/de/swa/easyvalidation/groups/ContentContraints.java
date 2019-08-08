package de.swa.easyvalidation.groups;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.json.JsonSerializable;
import de.swa.easyvalidation.json.JsonUtil;

import static de.swa.easyvalidation.json.JsonUtil.asKey;
import static de.swa.easyvalidation.json.JsonUtil.asObject;

public class ContentContraints {

    private final Constraint contentConstraint;
    private final ConstraintsTopGroup constraintsTopGroup;

    public ContentContraints(final Constraint contentConstraint, final ConstraintsTopGroup constraintsTopGroup) {
        this.contentConstraint = contentConstraint;
        this.constraintsTopGroup = constraintsTopGroup;
    }

    public Constraint getContentConstraint() {
        return contentConstraint;
    }

    public ConstraintsTopGroup getConstraintsTopGroup() {
        return constraintsTopGroup;
    }

}

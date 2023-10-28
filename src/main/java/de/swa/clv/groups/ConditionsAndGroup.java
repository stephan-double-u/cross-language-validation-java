package de.swa.clv.groups;

import de.swa.clv.constraints.Condition;

public class ConditionsAndGroup extends ConditionsGroup {

    protected ConditionsAndGroup(final Condition... andedConditions) {
        conditions = andedConditions;
    }

}

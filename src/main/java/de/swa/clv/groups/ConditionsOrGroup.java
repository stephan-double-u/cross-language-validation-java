package de.swa.clv.groups;

import de.swa.clv.constraints.Condition;

public class ConditionsOrGroup extends ConditionsGroup {

    protected ConditionsOrGroup(final Condition... oredConditions) {
        conditions = oredConditions;
    }

}

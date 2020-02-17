package de.swa.clv.groups;

import de.swa.clv.constraints.PropConstraint;
import de.swa.clv.json.JsonSerializable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.asArray;
import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.asObject;
import static de.swa.clv.json.JsonUtil.quoted;

public abstract class RelationsSubGroup implements JsonSerializable {

    private static Logger log = LoggerFactory.getLogger(RelationsSubGroup.class);

    protected PropConstraint[] propConstraints;

    public static final String ANDED_GROUP_JSON_VALUE = "AND";
    public static final String ORED_GROUP_JSON_VALUE = "OR";

    public static RelationsAndGroup AND(final PropConstraint... propConstraints) {
        return new RelationsAndGroup(propConstraints);
    }

    public static RelationsOrGroup OR(final PropConstraint... propConstraints) {
        return new RelationsOrGroup(propConstraints);
    }

    public PropConstraint[] getPropConstraints() {
        return propConstraints;
    }

    @Override
    public String serializeToJson() {
        String operator = (this instanceof RelationsAndGroup) ? LogicalOperator.AND.name() : LogicalOperator.OR.name();
        final String refsAsJson = Arrays.stream(propConstraints).map(r -> r.serializeToJson()).collect(Collectors.joining(","));
        return asObject(asKey("operator") + quoted(operator) + "," + asKey("constraints") + asArray(refsAsJson));
    }

}

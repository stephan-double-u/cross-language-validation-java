package de.swa.easyvalidation.constraints;

import static de.swa.easyvalidation.json.JsonUtil.*;

import de.swa.easyvalidation.json.JsonSerializable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ConstraintRef implements JsonSerializable {

    private final String property;
    private final Constraint constraint;

    ConstraintRef(final String property, final Constraint constraint) {
        this.property = property;
        this.constraint = constraint;
    }

    public String getProperty() {
        return property;
    }

    public Constraint getConstraint() {
        return constraint;
    }

    @Override
    public String serializeToJson() {
        return asObject(asKey("property") + quoted(property) + "," + constraint.serializeToJson());
    }


}

package de.swa.easyvalidation.constraints;

import static de.swa.easyvalidation.json.JsonUtil.*;

import de.swa.easyvalidation.json.JsonSerializable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PropConstraint implements JsonSerializable {

    private final String property;
    private final ConstraintRoot constraint;

    PropConstraint(final String property, final ConstraintRoot constraint) {
        this.property = property;
        this.constraint = constraint;
    }

    public String getProperty() {
        return property;
    }

    public ConstraintRoot getConstraint() {
        return constraint;
    }

    @Override
    public String serializeToJson() {
        return asObject(asKey("property") + quoted(property) + "," + constraint.serializeToJson());
    }


}

package de.swa.easyvalidation.constraints;

import static de.swa.easyvalidation.json.JsonUtil.*;

import de.swa.easyvalidation.EasyValidationTesting;
import de.swa.easyvalidation.json.JsonSerializable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ConstraintRef implements JsonSerializable {

    private static Logger log = LoggerFactory.getLogger(ConstraintRef.class);

    private String property;
    private Constraint constraint;

    protected ConstraintRef(String property, Constraint constraint) {
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
        return asObject(asKey("property") + quoted(property) + "," + asKey("constraint") + constraint.serializeToJson());
    }


}
package de.swa.clv.constraints;

import static de.swa.clv.json.JsonUtil.*;

import de.swa.clv.json.JsonSerializable;

import java.util.Objects;

public final class Condition implements JsonSerializable {

    private final String property;
    private final Constraint constraint;

    public static Condition of(final String propertyName, final Constraint constraint) {
        return new Condition(propertyName, constraint);
    }

    private Condition(String property, Constraint constraint) {
        this.property = property;
        this.constraint = constraint;
    }

    @Override
    public String serializeToJson() {
        return asObject(asKey("property") + quoted(property) + "," + asKey("constraint") + asObject(
                constraint.serializeToJson()));
    }

    public String property() {
        return property;
    }

    public Constraint constraint() {
        return constraint;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (obj == null || obj.getClass() != this.getClass())
            return false;
        var that = (Condition) obj;
        return Objects.equals(this.property, that.property) &&
                Objects.equals(this.constraint, that.constraint);
    }

    @Override
    public int hashCode() {
        return Objects.hash(property, constraint);
    }

    @Override
    public String toString() {
        return "ConditionConstraint[" +
                "property=" + property + ", " +
                "constraint=" + constraint + ']';
    }

}

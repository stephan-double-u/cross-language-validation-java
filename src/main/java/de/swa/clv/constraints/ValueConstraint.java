package de.swa.clv.constraints;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.quoted;

public abstract class ValueConstraint extends Constraint implements ValueComparer {

    @Override
    public boolean isSupportedType(Class<?> clazz) {
        return true;
    }

    @Override
    public String serializeToJson() {
        return asKey("type") + quoted(getToken());
    }

}

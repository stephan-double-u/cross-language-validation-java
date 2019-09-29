package de.swa.easyvalidation.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EqualsNotNull extends Equals {

    private static Logger log = LoggerFactory.getLogger(EqualsNotNull.class);

    EqualsNotNull() {
    }

    @Override
    String getType() {
        return "EQUALS_NOT_NULL";
    }

    @Override
    public boolean validate(final Object object, final Object ignored) {
        return object != null;
    }

}

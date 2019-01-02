package de.swa.easyvalidation.constraints;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static de.swa.easyvalidation.json.JsonUtil.*;

import java.util.Arrays;
import java.util.Comparator;

public class LessThan extends Constraint {

    private static Logger log = LoggerFactory.getLogger(LessThan.class);

    private static final String type = "LESS_THAN";

    private Comparator<String> comparator = null;

    public static LessThanString value(String value) {
        LessThanString constraint = new LessThanString();
        constraint.setStringValues(Arrays.asList(value));
        return constraint;
    }

    @Override
    public boolean validate(Object object, Object contraintObject) {
        String stringObject = (String) object;
        for (Object value : getValues()) {
            String stringValue = (String) value;
            if (comparator == null && stringObject.compareTo(stringValue) < 0
                    || comparator != null && comparator.compare(stringObject, stringValue) < 0) {
                log.debug("'" + object + "' is < than '" + value + "'");
                return true;
            }
        }
        log.debug(object + " is !< than any of " + getValues());
        return false;
    }

    @Override
    public String serializeToJson() {
        // TODO Auto-generated method stub
        return asObject(asKey("TODO") + quoted("TODO"));
    }

    public void setComparator(Comparator<String> comparator) {
        this.comparator = comparator;
    }

    @Override
    public boolean isSupportedType(Class<?> clazz) {
        // TODO Auto-generated method stub
        return String.class == clazz;
    }

}

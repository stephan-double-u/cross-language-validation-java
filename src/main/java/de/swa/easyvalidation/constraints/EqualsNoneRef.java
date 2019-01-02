package de.swa.easyvalidation.constraints;

import java.util.Arrays;

import de.swa.easyvalidation.EasyValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EqualsNoneRef extends Equals {

    private static Logger log = LoggerFactory.getLogger(EqualsNoneRef.class);

    EqualsNoneRef(String... properties) {
        setStringValues(Arrays.asList(properties));
    }

    @Override
    String getType() {
        return "EQUALS_NONE_REF";
    }

    @Override
    public boolean validateArgumentsOrFail(Class<?> typeClass) {
        getValues().forEach(refPropertyName -> EasyValidator.validateProperty((String) refPropertyName, typeClass));
        return true;
    }

    @Override
    public boolean validate(Object valueToValidate, Object contraintObject) {
        for (Object property : getValues()) {
            //TODO allow only non-indexed properties resp. 'single index'!?
            Object referencedValue = EasyValidator.getPropertyResultObject((String) property, contraintObject);
            if (valueToValidate.equals(referencedValue)) {
                log.debug("" + valueToValidate + " does NOT equals none referenced property " + property);
                return false;
            }
        }
        log.debug("" + valueToValidate + " does equals none referenced property of " + getValues());
        return true;
    }

    public static void main(String[] args) {
        EqualsNoneRef constraint = Equals.noneRef("bar.zoo");
        log.debug(constraint.serializeToJson());
        Foo foo = new Foo(new Bar("baz"));
        // Validating caches the getZoo() method!
        EasyValidator.validateProperty("bar.zoo", Foo.class);
        log.debug("" + constraint.validate("baz", foo));
    }

    public static class Foo {
        private Bar bar;
        public Foo(Bar bar) {
            super();
            this.bar = bar;
        }
        public Bar getBar() {
            return bar;
        }
    }
    public static class Bar {
        private String zoo;
        public Bar(String zoo) {
            this.zoo = zoo;
        }
        public String getZoo() {
            return zoo;
        }
    }
    
}

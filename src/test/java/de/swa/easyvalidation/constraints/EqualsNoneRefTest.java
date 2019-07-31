package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.EasyValidator;
import org.junit.Test;

import static org.junit.Assert.*;

public class EqualsNoneRefTest {

    // TODO more and better tests ...

    @Test
    public void validate() {
        EqualsNoneRef constraint = Equals.noneRef("bar.zoo");
        assertEquals("\"type\":\"EQUALS_NONE_REF\",\"values\":[\"bar.zoo\"]", constraint.serializeToJson());

        Foo foo = new Foo(new Bar("baz"));
        // Validating caches the getZoo() method!
        EasyValidator.validateProperty("bar.zoo", Foo.class);
        assertFalse(constraint.validate("baz", foo));
    }

    protected static class Foo {
        private Bar bar;
        public Foo(Bar bar) {
            super();
            this.bar = bar;
        }
        public Bar getBar() {
            return bar;
        }
    }

    protected static class Bar {
        private String zoo;
        public Bar(String zoo) {
            this.zoo = zoo;
        }
        public String getZoo() {
            return zoo;
        }
    }
}
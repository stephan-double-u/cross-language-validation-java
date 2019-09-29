package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.EasyValidator;
import de.swa.easyvalidation.test.Util;
import org.hamcrest.core.StringContains;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.*;

public class EqualsNoneRefTest {

    private static Foo foo = new Foo(new Bar("baz", Enum.VALID, (short) 1, true));

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Test
    public void exceptionIfStringIsNull() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Null values are not allowed"));
        Equals.noneRef((String) null);
    }

    @Test
    public void validateString() {
        EqualsNoneRef constraint = Equals.noneRef("bar.stringProp");
        // Validating caches the getStringProp() method!
        EasyValidator.validateProperty("bar.stringProp", Foo.class);
        assertTrue(constraint.validate("invalid", foo));
    }

    @Test
    public void validateStringVsEnum() {
        EqualsNoneRef constraint = Equals.noneRef("bar.enumProp");
        // Validating caches the getEnumProp() method!
        EasyValidator.validateProperty("bar.enumProp", Foo.class);
        assertTrue(constraint.validate("INVALID", foo));
    }

    @Test
    public void validateEnumVsEnum() {
        EqualsNoneRef constraint = Equals.noneRef("bar.enumProp");
        // Validating caches the getEnumProp() method!
        EasyValidator.validateProperty("bar.enumProp", Foo.class);
        assertTrue(constraint.validate(Enum.INVALID, foo));
    }

    @Test
    public void validateNumber() {
        EqualsNoneRef constraint = Equals.noneRef("bar.intProp");
        // Validating caches the getIntProp() method!
        EasyValidator.validateProperty("bar.intProp", EqualsNoneRefTest.Foo.class);
        assertTrue(constraint.validate(-999, foo));
    }

    @Test
    public void validateBoolean() {
        EqualsNoneRef constraint = Equals.noneRef("bar.booleanProp");
        // Validating caches the getStringProp() method!
        EasyValidator.validateProperty("bar.booleanProp", EqualsNoneRefTest.Foo.class);
        assertTrue(constraint.validate(false, foo));
    }

    @Test
    public void serializeToJson() {
        EqualsNoneRef constraint = Equals.noneRef("bar.stringProp");
        assertEquals(Util.doubleQuote("'type':'EQUALS_NONE_REF','values':['bar.stringProp']"), constraint.serializeToJson());
    }

    protected static class Foo {
        private Bar bar;
        public Foo(Bar bar) {
            this.bar = bar;
        }
        public Bar getBar() {
            return bar;
        }
    }

    protected static class Bar {
        private String stringProp;
        private Enum enumProp;
        private int intProp;
        private Boolean booleanProp;
        public Bar(String stringProp, Enum enumProp, int intProp, Boolean booleanProp) {
            this.stringProp = stringProp;
            this.enumProp = enumProp;
            this.intProp = intProp;
            this.booleanProp = booleanProp;
        }
        public String getStringProp() {
            return stringProp;
        }
        public Enum getEnumProp() {
            return enumProp;
        }
        public int getIntProp() {
            return intProp;
        }
        public Boolean getBooleanProp() {
            return booleanProp;
        }
    }

    enum Enum {
        VALID, INVALID
    }
}
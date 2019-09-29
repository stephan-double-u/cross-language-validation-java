package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.EasyValidator;
import de.swa.easyvalidation.test.Util;
import org.hamcrest.core.StringContains;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.net.URL;

import static org.junit.Assert.*;

public class EqualsAnyRefTest {

    private static Foo foo = new Foo(new Bar("baz", Enum.ABC, (short) 1, true));

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Test
    public void exceptionIfStringIsNull() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Null values are not allowed"));
        Equals.anyRef((String) null);
    }

    @Test
    public void validateString() {
        EqualsAnyRef constraint = Equals.anyRef("bar.stringProp");
        // Validating caches the getStringProp() method!
        EasyValidator.validateProperty("bar.stringProp", Foo.class);
        assertTrue(constraint.validate("baz", foo));
    }

    @Test
    public void validateStringVsEnum() {
        EqualsAnyRef constraint = Equals.anyRef("bar.enumProp");
        // Validating caches the getEnumProp() method!
        EasyValidator.validateProperty("bar.enumProp", Foo.class);
        assertTrue(constraint.validate("ABC", foo));
    }

    @Test
    public void validateEnumVsEnum() {
        EqualsAnyRef constraint = Equals.anyRef("bar.enumProp");
        // Validating caches the getEnumProp() method!
        EasyValidator.validateProperty("bar.enumProp", Foo.class);
        assertTrue(constraint.validate(Enum.ABC, foo));
    }

    @Test
    public void validateNumber() {
        EqualsAnyRef constraint = Equals.anyRef("bar.intProp");
        // Validating caches the getIntProp() method!
        EasyValidator.validateProperty("bar.intProp", Foo.class);
        assertTrue(constraint.validate(1, foo));
    }

    @Test
    public void validateBoolean() {
        EqualsAnyRef constraint = Equals.anyRef("bar.booleanProp");
        // Validating caches the getStringProp() method!
        EasyValidator.validateProperty("bar.booleanProp", Foo.class);
        assertTrue(constraint.validate(true, foo));
    }

    @Test
    public void serializeToJson() {
        EqualsAnyRef constraint = Equals.anyRef("bar.stringProp");
        assertEquals(Util.doubleQuote("'type':'EQUALS_ANY_REF','values':['bar.stringProp']"), constraint.serializeToJson());
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
        ABC
    }
}
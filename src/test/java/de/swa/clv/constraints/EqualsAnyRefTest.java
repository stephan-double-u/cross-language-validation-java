package de.swa.clv.constraints;

import de.swa.clv.Validator;
import de.swa.clv.test.Util;
import org.hamcrest.core.StringContains;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.time.Instant;
import java.time.LocalDate;
import java.time.Month;
import java.util.Date;

import static org.junit.Assert.*;

public class EqualsAnyRefTest {

    private static Foo foo = new Foo(new Bar("baz", Enum.ABC, (short) 1, true,
            LocalDate.of(2000, Month.JANUARY, 1),
            new Date(LocalDate.of(2000, Month.JANUARY, 1).toEpochDay())));

    @Test
    public void validateNullVsNull() {
        EqualsAnyRef constraint = Equals.anyRef(null);
        assertTrue(constraint.validate(null, foo));
    }

    @Test
    public void validateString() {
        EqualsAnyRef constraint = Equals.anyRef("bar.stringProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.stringProp", Foo.class);
        assertTrue(constraint.validate("baz", foo));
    }

    @Test
    public void validateStringVsEnum() {
        EqualsAnyRef constraint = Equals.anyRef("bar.enumProp");
        // Validating caches the getEnumProp() method!
        Validator.instance().validateProperty("bar.enumProp", Foo.class);
        assertTrue(constraint.validate("ABC", foo));
    }

    @Test
    public void validateEnumVsEnum() {
        EqualsAnyRef constraint = Equals.anyRef("bar.enumProp");
        // Validating caches the getEnumProp() method!
        Validator.instance().validateProperty("bar.enumProp", Foo.class);
        assertTrue(constraint.validate(Enum.ABC, foo));
    }

    @Test
    public void validateNumber() {
        EqualsAnyRef constraint = Equals.anyRef("bar.intProp");
        // Validating caches the getIntProp() method!
        Validator.instance().validateProperty("bar.intProp", Foo.class);
        assertTrue(constraint.validate(1, foo));
    }

    @Test
    public void validateBoolean() {
        EqualsAnyRef constraint = Equals.anyRef("bar.booleanProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.booleanProp", Foo.class);
        assertTrue(constraint.validate(true, foo));
    }

    @Test
    public void validateLocalDate() {
        EqualsAnyRef constraint = Equals.anyRef("bar.localDateProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.localDateProp", Foo.class);
        assertTrue(constraint.validate(LocalDate.of(2000, Month.JANUARY, 1), foo));
    }

    @Test
    public void validateLocalDateFail() {
        EqualsAnyRef constraint = Equals.anyRef("bar.localDateProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.localDateProp", Foo.class);
        assertFalse(constraint.validate(LocalDate.of(1999, Month.DECEMBER, 31), foo));
    }

    @Test
    public void validatelDate() {
        EqualsAnyRef constraint = Equals.anyRef("bar.dateProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.dateProp", Foo.class);
        assertTrue(constraint.validate(new Date(LocalDate.of(2000, Month.JANUARY, 1).toEpochDay()), foo));
    }

    @Test
    public void validateDateFail() {
        EqualsAnyRef constraint = Equals.anyRef("bar.dateProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.dateProp", Foo.class);
        assertFalse(constraint.validate(new Date(LocalDate.of(1999, Month.DECEMBER, 31).toEpochDay()), foo));
    }

    @Test
    public void validateIndexedProperty() {
        EqualsAnyRef constraint = Equals.anyRef("enum2Prop.nestedEnums[*]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("enum2Prop.nestedEnums[*]", Foo.class);
        assertTrue(constraint.validate(Enum.GHI, new Foo(Enum2.ONE)));
    }

    @Test
    public void validateIndexedPropertyFail() {
        EqualsAnyRef constraint = Equals.anyRef("enum2Prop.nestedEnums[*]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("enum2Prop.nestedEnums[*]", Foo.class);
        assertFalse(constraint.validate(Enum.JKL, new Foo(Enum2.ONE)));
    }

    @Test
    public void validateNoIndexedPropertyFail() {
        EqualsAnyRef constraint = Equals.anyRef("enum2Prop.nestedEnums[*]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("enum2Prop.nestedEnums[*]", Foo.class);
        assertFalse(constraint.validate(Enum.JKL, new Foo(Enum2.TWO)));
    }

    @Test
    public void serializeToJson() {
        EqualsAnyRef constraint = Equals.anyRef("bar.stringProp");
        assertEquals(Util.doubleQuote("'type':'EQUALS_ANY_REF','values':['bar.stringProp']"), constraint.serializeToJson());
    }

    protected static class Foo {
        private Bar bar;
        private Enum2 enum2Prop;
        private Object nullProp = null;

        public Foo(Bar bar) {
            this.bar = bar;
        }
        public Foo(Enum2 enum2Prop) {
            this.enum2Prop = enum2Prop;
        }
        public Bar getBar() {
            return bar;
        }
        public Enum2 getEnum2Prop() {
            return enum2Prop;
        }
        public Object getNullProp() {
            return nullProp;
        }
    }

    protected static class Bar {
        private String stringProp;
        private Enum enumProp;
        private int intProp;
        private Boolean booleanProp;
        private LocalDate localDateProp;
        private Date dateProp;

        public Bar(String stringProp, Enum enumProp, int intProp, Boolean booleanProp, LocalDate localDateProp, Date dateProp) {
            this.stringProp = stringProp;
            this.enumProp = enumProp;
            this.intProp = intProp;
            this.booleanProp = booleanProp;
            this.localDateProp = localDateProp;
            this.dateProp = dateProp;
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
        public LocalDate getLocalDateProp() {
            return localDateProp;
        }
        public Date getDateProp() {
            return dateProp;
        }
    }

    enum Enum {
        ABC, DEF, GHI, JKL
    }

    public enum Enum2 {
        ONE(Enum.ABC, Enum.DEF, Enum.GHI),
        TWO();

        private Enum[] nestedEnums;

        Enum2(Enum... enums) {
            this.nestedEnums = enums;
        }

        public Enum[] getNestedEnums() {
            return nestedEnums;
        }
    }
}

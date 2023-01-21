package de.swa.clv.constraints;

import de.swa.clv.Validator;
import de.swa.clv.test.Util;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.time.Month;
import java.util.Date;

import static de.swa.clv.constraints.Constraint.EMPTY_VALUES_ERR_MESSAGE;
import static de.swa.clv.constraints.Constraint.NULL_VALUE_ERR_MESSAGE;
import static org.junit.jupiter.api.Assertions.*;

class EqualsNoneRefTest {

    private static final EqualsNoneRefTest.Foo foo = new EqualsNoneRefTest.Foo(
            new EqualsNoneRefTest.Bar("baz", EqualsNoneRefTest.Enum.ABC, (short) 1, true,
            LocalDate.of(2000, Month.JANUARY, 1),
            new Date(LocalDate.of(2000, Month.JANUARY, 1).toEpochDay())));


    @Test
    void nullNotAllowed() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                Equals::noneRef);
        assertEquals(EMPTY_VALUES_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void noValuesNotAllowed() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Equals.noneRef(null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void nullValuesNotAllowed() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Equals.noneRef(null, "someProp"));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void validateNoneRefVsNull() {
        EqualsNoneRef constraint = Equals.noneRef("bar.stringProp");
        assertTrue(constraint.validate(null, foo));
    }

    @Test
    void validateNoneRefNorNullVsNull() {
        EqualsNoneRef constraint = Equals.noneRefNorNull("bar.stringProp");
        assertFalse(constraint.validate(null, foo));
    }

    @Test
    void validateString() {
        EqualsNoneRef constraint = Equals.noneRef( "bar.stringProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.stringProp", Foo.class);
        assertTrue(constraint.validate("invalid", foo));
    }

    @Test
    void validateStringVsEnum() {
        EqualsNoneRef constraint = Equals.noneRef("bar.enumProp");
        // Validating caches the getEnumProp() method!
        Validator.instance().validateProperty("bar.enumProp", Foo.class);
        assertTrue(constraint.validate("INVALID", foo));
    }

    @Test
    void validateEnumVsEnum() {
        EqualsNoneRef constraint = Equals.noneRef("bar.enumProp");
        // Validating caches the getEnumProp() method!
        Validator.instance().validateProperty("bar.enumProp", Foo.class);
        assertTrue(constraint.validate(Enum.DEF, foo));
    }

    @Test
    void validateNumber() {
        EqualsNoneRef constraint = Equals.noneRef("bar.intProp");
        // Validating caches the getIntProp() method!
        Validator.instance().validateProperty("bar.intProp", EqualsNoneRefTest.Foo.class);
        assertTrue(constraint.validate(-999, foo));
    }

    @Test
    void validateBoolean() {
        EqualsNoneRef constraint = Equals.noneRef("bar.booleanProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.booleanProp", EqualsNoneRefTest.Foo.class);
        assertTrue(constraint.validate(false, foo));
    }

    @Test
    void validateLocalDate() {
        EqualsNoneRef constraint = Equals.noneRef("bar.localDateProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.localDateProp", EqualsNoneRefTest.Foo.class);
        assertTrue(constraint.validate(LocalDate.of(1999, Month.DECEMBER, 31), foo));
    }

    @Test
    void validateLocalDateFail() {
        EqualsNoneRef constraint = Equals.noneRef("bar.localDateProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.localDateProp", EqualsNoneRefTest.Foo.class);
        assertFalse(constraint.validate(LocalDate.of(2000, Month.JANUARY, 1), foo));
    }

    @Test
    void validatelDate() {
        EqualsNoneRef constraint = Equals.noneRef("bar.dateProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.dateProp", EqualsNoneRefTest.Foo.class);
        assertTrue(constraint.validate(new Date(LocalDate.of(1999, Month.DECEMBER, 31).toEpochDay()), foo));
    }

    @Test
    void validateDateFail() {
        EqualsNoneRef constraint = Equals.noneRef("bar.dateProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.dateProp", EqualsNoneRefTest.Foo.class);
        assertFalse(constraint.validate(new Date(LocalDate.of(2000, Month.JANUARY, 1).toEpochDay()), foo));
    }

    @Test
    void validateIndexedProperty() {
        EqualsNoneRef constraint = Equals.noneRef("enum2Prop.nestedEnums[*]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("enum2Prop.nestedEnums[*]", Foo.class);
        assertTrue(constraint.validate(Enum.JKL, new Foo(Enum2.ONE)));
    }

    @Test
    void validateIndexedPropertyFail() {
        EqualsNoneRef constraint = Equals.noneRef("enum2Prop.nestedEnums[*]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("enum2Prop.nestedEnums[*]", Foo.class);
        assertFalse(constraint.validate(Enum.GHI, new Foo(Enum2.ONE)));
    }

    @Test
    void validateNoIndexedProperty() {
        EqualsNoneRef constraint = Equals.noneRef("enum2Prop.nestedEnums[*]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("enum2Prop.nestedEnums[*]", Foo.class);
        assertTrue(constraint.validate(Enum.GHI, new Foo(Enum2.TWO)));
    }

    @Test
    void serializeToJsonNorNull() {
        EqualsNoneRef constraint = Equals.noneRefNorNull("bar.stringProp");
        assertEquals("""
                "type":"EQUALS_NONE_REF","values":["bar.stringProp"],"nullEqualsTo":false""",
                constraint.serializeToJson());
    }

    @Test
    void serializeToJson() {
        EqualsNoneRef constraint = Equals.noneRef("bar.stringProp");
        assertEquals("""
                "type":"EQUALS_NONE_REF","values":["bar.stringProp"]""", constraint.serializeToJson());
    }

    @Test
    void serializeToJson_ofUpdate() {
        EqualsNoneRef constraint = Equals.noneRef("bar.stringProp").ofUpdate();
        assertEquals("""
                "type":"EQUALS_NONE_REF","values":["bar.stringProp"],"refTarget":"UPDATE_ENTITY\"""",
                constraint.serializeToJson());
    }

    @Test
    void serializeToJson_ofCurrent() {
        EqualsNoneRef constraint = Equals.noneRef("bar.stringProp").ofCurrent();
        assertEquals("""
                "type":"EQUALS_NONE_REF","values":["bar.stringProp"],"refTarget":"CURRENT_ENTITY\"""",
                constraint.serializeToJson());
    }

    protected static class Foo {
        private Bar bar;
        private Enum2 enum2Prop;

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
    }

    protected static class Bar {
        private final String stringProp;
        private final Enum enumProp;
        private final int intProp;
        private final Boolean booleanProp;
        private final LocalDate localDateProp;
        private final Date dateProp;

        public Bar(String stringProp, Enum enumProp, int intProp, Boolean booleanProp, LocalDate localDateProp,
                Date dateProp) {
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

        private final Enum[] nestedEnums;

        Enum2(Enum... enums) {
            this.nestedEnums = enums;
        }

        public Enum[] getNestedEnums() {
            return nestedEnums;
        }
    }
}

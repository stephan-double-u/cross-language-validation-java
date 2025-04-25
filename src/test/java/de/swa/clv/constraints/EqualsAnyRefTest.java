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

class EqualsAnyRefTest {

    private static Foo foo = new Foo(new Bar("baz", Enum.ABC, 1, 2L, true,
            LocalDate.of(2000, Month.JANUARY, 1),
            new Date(LocalDate.of(2000, Month.JANUARY, 1).toEpochDay())));

    @Test
    void nullNotAllowed() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                Equals::anyRef);
        assertEquals(EMPTY_VALUES_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void noValuesNotAllowed() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Equals.anyRef((String) null));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void nullValuesNotAllowed() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Equals.anyRef(null, "someProp"));
        assertEquals(NULL_VALUE_ERR_MESSAGE, ex.getMessage());
    }

    @Test
    void validateAnyRefVsNull() {
        EqualsAnyRef constraint = Equals.anyRef("bar.stringProp");
        assertFalse(constraint.validate(null, foo));
    }

    @Test
    void validateAnyRefOrNullVsNull() {
        EqualsAnyRef constraint = Equals.anyRefOrNull("bar.stringProp");
        assertTrue(constraint.validate(null, foo));
    }

    @Test
    void validateString() {
        EqualsAnyRef constraint = Equals.anyRef("bar.stringProp").ofUpdate();
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.stringProp", Foo.class);
        assertTrue(constraint.validate("baz", foo));
    }

    @Test
    void validateStringVsEnum() {
        EqualsAnyRef constraint = Equals.anyRef("bar.enumProp");
        // Validating caches the getEnumProp() method!
        Validator.instance().validateProperty("bar.enumProp", Foo.class);
        assertTrue(constraint.validate("ABC", foo));
    }

    @Test
    void validateEnumVsEnum() {
        EqualsAnyRef constraint = Equals.anyRef("bar.enumProp");
        // Validating caches the getEnumProp() method!
        Validator.instance().validateProperty("bar.enumProp", Foo.class);
        assertTrue(constraint.validate(Enum.ABC, foo));
    }

    @Test
    void validateNumber() {
        EqualsAnyRef constraint = Equals.anyRef("bar.intProp", "bar.longProp");
        // Validating caches the getIntProp() method!
        Validator.instance().validateProperty("bar.intProp", Foo.class);
        Validator.instance().validateProperty("bar.longProp", Foo.class);
        assertTrue(constraint.validate(2, foo));
    }

    @Test
    void validateBoolean() {
        EqualsAnyRef constraint = Equals.anyRef("bar.booleanProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.booleanProp", Foo.class);
        assertTrue(constraint.validate(true, foo));
    }

    @Test
    void validateLocalDate() {
        EqualsAnyRef constraint = Equals.anyRef("bar.localDateProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.localDateProp", Foo.class);
        assertTrue(constraint.validate(LocalDate.of(2000, Month.JANUARY, 1), foo));
    }

    @Test
    void validateLocalDateFail() {
        EqualsAnyRef constraint = Equals.anyRef("bar.localDateProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.localDateProp", Foo.class);
        assertFalse(constraint.validate(LocalDate.of(1999, Month.DECEMBER, 31), foo));
    }

    @Test
    void validatelDate() {
        EqualsAnyRef constraint = Equals.anyRef("bar.dateProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.dateProp", Foo.class);
        assertTrue(constraint.validate(new Date(LocalDate.of(2000, Month.JANUARY, 1).toEpochDay()), foo));
    }

    @Test
    void validateDateFail() {
        EqualsAnyRef constraint = Equals.anyRef("bar.dateProp");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.dateProp", Foo.class);
        assertFalse(constraint.validate(new Date(LocalDate.of(1999, Month.DECEMBER, 31).toEpochDay()), foo));
    }

    @Test
    void validateIndexedProperty() {
        EqualsAnyRef constraint = Equals.anyRef("enum2Prop.nestedEnums[*]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("enum2Prop.nestedEnums[*]", Foo.class);
        assertTrue(constraint.validate(Enum.GHI, new Foo(Enum2.ONE)));
    }

    @Test
    void validateIndexedPropertyFail() {
        EqualsAnyRef constraint = Equals.anyRef("enum2Prop.nestedEnums[*]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("enum2Prop.nestedEnums[*]", Foo.class);
        assertFalse(constraint.validate(Enum.JKL, new Foo(Enum2.ONE)));
    }

    @Test
    void validateNoIndexedPropertyFail() {
        EqualsAnyRef constraint = Equals.anyRef("enum2Prop.nestedEnums[*]");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("enum2Prop.nestedEnums[*]", Foo.class);
        assertFalse(constraint.validate(Enum.JKL, new Foo(Enum2.TWO)));
    }

    @Test
    void validateToStringForClass() {
        EqualsAnyRef constraint = Equals.anyRef("bar.longProp.toString");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("bar.longProp.toString", Foo.class);
        assertTrue(constraint.validate("2", foo));
    }

    @Test
    void validateToStringForRecord() {
        var rec = new Record(2L);
        EqualsAnyRef constraint = Equals.anyRef("longProp.toString");
        // Validating caches the getStringProp() method!
        Validator.instance().validateProperty("longProp.toString", Record.class);
        assertTrue(constraint.validate("2", rec));
    }

    @Test
    void serializeToJsonOrNull() {
        EqualsAnyRef constraint = Equals.anyRefOrNull("bar.stringProp");
        assertEquals("""
                "type":"EQUALS_ANY_REF","values":["bar.stringProp"],"nullEqualsTo":true""",
                constraint.serializeToJson());
    }

    @Test
    void serializeToJson() {
        EqualsAnyRef constraint = Equals.anyRef("bar.stringProp");
        assertEquals("""
                "type":"EQUALS_ANY_REF","values":["bar.stringProp"]""", constraint.serializeToJson());
    }

    @Test
    void serializeToJson_ofUpdate() {
        EqualsAnyRef constraint = Equals.anyRef("bar.stringProp").ofUpdate();
        assertEquals("""
                "type":"EQUALS_ANY_REF","values":["bar.stringProp"],"refTarget":"UPDATE_ENTITY\"""",
                constraint.serializeToJson());
    }

    @Test
    void serializeToJson_ofCurrent() {
        EqualsAnyRef constraint = Equals.anyRef("bar.stringProp").ofCurrent();
        assertEquals("""
                "type":"EQUALS_ANY_REF","values":["bar.stringProp"],"refTarget":"CURRENT_ENTITY\"""",
                constraint.serializeToJson());
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
        private Long longProp;
        private Boolean booleanProp;
        private LocalDate localDateProp;
        private Date dateProp;

        public Bar(String stringProp, Enum enumProp, int intProp, Long longProp, Boolean booleanProp, LocalDate localDateProp, Date dateProp) {
            this.stringProp = stringProp;
            this.enumProp = enumProp;
            this.intProp = intProp;
            this.longProp = longProp;
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
        public Long getLongProp() {return longProp;}
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

    public record Record(Long longProp) {}
}

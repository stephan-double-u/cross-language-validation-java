package de.swa.clv;

import de.swa.clv.constraints.Equals;
import org.junit.Ignore;
import org.junit.Test;

import java.math.BigInteger;
import java.time.LocalDate;
import java.util.*;

import static org.junit.Assert.*;

public class ValidatorTest {

    /*
     * Testing validateProperty
     */

    @Test
    public void validateProperty_simplePropertyInherited() {
        try {
            Validator.instance().validateProperty("id", ClassUnderTest.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    public void validateProperty_simpleProperty() {
        try {
            Validator.instance().validateProperty("stringProp", ClassUnderTest.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    public void validateProperty_nestedProperty() {
        try {
            Validator.instance().validateProperty("subClassProp.stringProp", ClassUnderTest.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    public void validateProperty_nestedIndexedArrayProperty() {
        try {
            Validator.instance().validateProperty("subClassProp.stringArrayProp[0]", ClassUnderTest.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    public void validateProperty_indexedArrayIndexedListProperty() {
        try {
            Validator.instance().validateProperty("subClassArrayProp[0].stringListProp[999]", ClassUnderTest.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test(expected = IllegalArgumentException.class)
    public void validateProperty_notexisting() {
        Validator.instance().validateProperty("notexisting", ClassUnderTest.class);
    }


    static ValidationRules<ClassUnderTest> classUnderTestRules = new ValidationRules<>(ClassUnderTest.class);
    static {
        classUnderTestRules.mandatory("stringProp");
        classUnderTestRules.immutable("enumProp");
    }

    /*
     * Testing validateMandatoryRules
     */

    @Test
    public void validateMandatoryRules_true() {
        ClassUnderTest object = new ClassUnderTest("someString", null);
        List<String> errors = Validator.instance().validateMandatoryRules(object, classUnderTestRules);
        assertTrue(errors.isEmpty());
    }

    @Test
    public void validateMandatoryRules_false() {
        ClassUnderTest object = new ClassUnderTest(null, null);
        List<String> errors = Validator.instance().validateMandatoryRules(object, classUnderTestRules);
        assertEquals(Arrays.asList("error.validation.mandatory.classundertest.stringProp"), errors);
    }

    /*
     * Testing validateImmutableRules
     */

    @Test
    public void validateImmutableRules_sameEnums() {
        ClassUnderTest original = new ClassUnderTest("someString", SomeEnum.ONE);
        ClassUnderTest modified1 = new ClassUnderTest("otherString", SomeEnum.ONE);
        List<String> errors = Validator.instance().validateImmutableRules(original, modified1, classUnderTestRules);
        assertTrue(errors.isEmpty());
    }

    @Test
    public void validateImmutableRules_nullVsNull() {
        ClassUnderTest original = new ClassUnderTest("someString", null);
        ClassUnderTest modified1 = new ClassUnderTest("otherString", null);
        List<String> errors = Validator.instance().validateImmutableRules(original, modified1, classUnderTestRules);
        assertTrue(errors.isEmpty());
    }

    @Test
    public void validateImmutableRules_differentEnums() {
        ClassUnderTest original = new ClassUnderTest("someString", SomeEnum.ONE);
        ClassUnderTest modified1 = new ClassUnderTest("otherString", SomeEnum.TWO);
        List<String> errors = Validator.instance().validateImmutableRules(original, modified1, classUnderTestRules);
        assertEquals(Arrays.asList("error.validation.immutable.classundertest.enumProp"), errors);
    }

    @Test
    public void validateImmutableRules_enumVsNull() {
        ClassUnderTest original = new ClassUnderTest("someString", SomeEnum.ONE);
        ClassUnderTest modified1 = new ClassUnderTest("otherString", null);
        List<String> errors = Validator.instance().validateImmutableRules(original, modified1, classUnderTestRules);
        assertEquals(Arrays.asList("error.validation.immutable.classundertest.enumProp"), errors);
    }

    @Test
    public void validateImmutableRules_nullVsEnum() {
        ClassUnderTest original = new ClassUnderTest("someString", null);
        ClassUnderTest modified1 = new ClassUnderTest("otherString", SomeEnum.TWO);
        List<String> errors = Validator.instance().validateImmutableRules(original, modified1, classUnderTestRules);
        assertEquals(Arrays.asList("error.validation.immutable.classundertest.enumProp"), errors);
    }


    @Test
    public void inflateIndexedProperty_starAndIncrement() {
        List<String> expected = Arrays.asList(
                "subClassArrayProp[0].stringArrayProp[2]",
                "subClassArrayProp[0].stringArrayProp[4]",
                "subClassArrayProp[1].stringArrayProp[2]");

        List<String> inflated = Validator.instance().inflateIndexedProperty("subClassArrayProp[*].stringArrayProp[2/2]", new ClassUnderTest());

        assertEquals(expected, inflated);
    }

    @Test
    public void inflateIndexedProperty_listAndRange() {
        List<String> expected = Arrays.asList(
                "subClassArrayProp[2].stringListProp[7]",
                "subClassArrayProp[2].stringListProp[8]",
                "subClassArrayProp[2].stringListProp[9]",
                "subClassArrayProp[3].stringListProp[7]",
                "subClassArrayProp[3].stringListProp[8]",
                "subClassArrayProp[3].stringListProp[9]");

        List<String> inflated = Validator.instance().inflateIndexedProperty("subClassArrayProp[2,3].stringListProp[7-9]", new ClassUnderTest());

        assertEquals(expected, inflated);
    }


    @Test
    public void validateContentRules_nestedIndexed() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.content("subClassArrayProp[*].stringArrayProp[0-3]", Equals.any("b2", "b3", "c2", "c3", "d2", "d3"));
        final List<String> errors = Validator.instance().validateContentRules(new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty());
    }


    class ClassUnderTest extends BaseClass implements Identifiable<Integer> {
        private String stringProp;
        private SomeEnum enumProp;
        private SubClass subClassProp = new SubClass("a1", new String[] {"b1", "c1"}, Arrays.asList("d1", "e1", "f1"));
        private SubClass[] subClassArrayProp = {
                new SubClass("a2", new String[] {"b2", "c2", "d2", "e2", "f2"}, Arrays.asList("g2", "h2")),
                new SubClass("a3", new String[] {"b3", "c3", "d3"}, Arrays.asList("e3", "f3", "g3", "h3"))};

        public ClassUnderTest() {
            super(1);
        }

        public ClassUnderTest(String stringProp, SomeEnum enumProp) {
            super(1);
            this.stringProp = stringProp;
            this.enumProp = enumProp;
        }

        @Override
        public Integer getId() {
            return super.getId() * 2;
        }

        public String getStringProp() {
            return stringProp;
        }

        public SomeEnum getEnumProp() {
            return enumProp;
        }

        public SubClass getSubClassProp() {
            return subClassProp;
        }

        public SubClass[] getSubClassArrayProp() {
            return subClassArrayProp;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            ClassUnderTest that = (ClassUnderTest) o;
            return Objects.equals(stringProp, that.stringProp) &&
                    enumProp == that.enumProp &&
                    Objects.equals(subClassProp, that.subClassProp) &&
                    Arrays.equals(subClassArrayProp, that.subClassArrayProp);
        }

        @Override
        public int hashCode() {
            int result = Objects.hash(stringProp, enumProp, subClassProp);
            result = 31 * result + Arrays.hashCode(subClassArrayProp);
            return result;
        }
    }

    class SubClass {
        private String stringProp = "someString";
        private String[] stringArrayProp;
        private List<String> stringListProp;


        public SubClass(String stringProp, String[] stringArrayProp, List<String> stringListProp) {
            this.stringProp = stringProp;
            this.stringArrayProp = stringArrayProp;
            this.stringListProp = stringListProp;
        }

        public String getStringProp() {
            return stringProp;
        }

        public String[] getStringArrayProp() {
            return stringArrayProp;
        }

        public List<String> getStringListProp() {
            return stringListProp;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            SubClass subClass = (SubClass) o;
            return Objects.equals(stringProp, subClass.stringProp) &&
                    Arrays.equals(stringArrayProp, subClass.stringArrayProp) &&
                    Objects.equals(stringListProp, subClass.stringListProp);
        }

        @Override
        public int hashCode() {
            int result = Objects.hash(stringProp, stringListProp);
            result = 31 * result + Arrays.hashCode(stringArrayProp);
            return result;
        }
    }

    class BaseClass {
        private final Integer id;
        public BaseClass(final Integer id) {
            super();
            this.id = id;
        }
        public Integer getId() {
            return id;
        }
    }

    interface Identifiable<T> {
        T getId();
    }
    enum SomeEnum {
        ONE, TWO, THREE
    }

}
package de.swa.clv;

import de.swa.clv.constraints.Condition;
import de.swa.clv.constraints.Equals;
import de.swa.clv.constraints.Permissions;
import org.junit.Before;
import org.junit.Test;

import java.time.LocalDate;
import java.time.ZoneId;
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


    private ValidationRules<ClassUnderTest> classUnderTestRules;
    @Before
    public void before() {
        classUnderTestRules = new ValidationRules<>(ClassUnderTest.class);
        classUnderTestRules.mandatory("stringProp");
        classUnderTestRules.immutable("enumProp");
    }

    @Test
    public void validate_permissions_string_vs_string_false() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("stringProp", Permissions.any("ONE"));
        ClassUnderTest object = new ClassUnderTest(null, null);
        List<String> errors = Validator.instance().validateMandatoryRules(object, UserPermissions.of("ONE"), rules);
        assertEquals(Arrays.asList("error.validation.mandatory.classundertest.stringProp"), errors);
    }

    @Test
    public void validate_permissions_enum_vs_string_false() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("stringProp", Permissions.any(SomeEnum.ONE));
        ClassUnderTest object = new ClassUnderTest(null, null);
        List<String> errors = Validator.instance().validateMandatoryRules(object, UserPermissions.of("ONE"), rules);
        assertEquals(Arrays.asList("error.validation.mandatory.classundertest.stringProp"), errors);
    }

    @Test
    public void validate_permissions_string_vs_enum_false() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("stringProp", Permissions.any("ONE"));
        ClassUnderTest object = new ClassUnderTest(null, null);
        List<String> errors = Validator.instance().validateMandatoryRules(object, UserPermissions.of(SomeEnum.ONE), rules);
        assertEquals(Arrays.asList("error.validation.mandatory.classundertest.stringProp"), errors);
    }

    @Test
    public void validate_permissions_enum_vs_enum_false() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("stringProp", Permissions.any(SomeEnum.ONE));
        ClassUnderTest object = new ClassUnderTest(null, null);
        List<String> errors = Validator.instance().validateMandatoryRules(object, UserPermissions.of(OtherEnum.ONE), rules);
        assertEquals(Arrays.asList("error.validation.mandatory.classundertest.stringProp"), errors);
    }

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

    @Test
    public void validateMandatoryRules_star() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("subClassArrayProp[*].stringProp");
        final List<String> errors = Validator.instance().validateMandatoryRules(new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty());
    }

    @Test
    public void validateMandatoryRules_starFail() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("subClassArrayProp[*].stringProp");

        ClassUnderTest classUnderTest = new ClassUnderTest();
        classUnderTest.subClassArrayProp[0].stringProp = null;

        final List<String> errors = Validator.instance().validateMandatoryRules(classUnderTest, rules);
        assertEquals(Arrays.asList("error.validation.mandatory.classundertest.subClassArrayProp[*].stringProp"), errors);
    }


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
    public void validateImmutableRules_star() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("subClassArrayProp[*].stringProp");
        final List<String> errors = Validator.instance().validateImmutableRules(new ClassUnderTest(), new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty());
    }

    @Test
    public void validateImmutableRules_starFail() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.immutable("subClassArrayProp[*].stringProp");

        ClassUnderTest modifiedObject = new ClassUnderTest();
        modifiedObject.subClassArrayProp[1].stringProp = "modified";

        final List<String> errors = Validator.instance().validateImmutableRules(new ClassUnderTest(), modifiedObject, rules);
        assertEquals(Arrays.asList("error.validation.immutable.classundertest.subClassArrayProp[*].stringProp"), errors);
    }

    @Test
    public void validateImmutableRules_utilDate() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.immutable("utilDate");

        final List<String> errors = Validator.instance().validateImmutableRules(new ClassUnderTest(), new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty());
    }

    @Test
    public void validateImmutableRules_utilDateFail() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.immutable("utilDate");

        ClassUnderTest modifiedObject = new ClassUnderTest();
        modifiedObject.setUtilDate(Date.from(LocalDate.of(2020, 1, 2 ).atStartOfDay(ZoneId.systemDefault()).toInstant()));

        final List<String> errors = Validator.instance().validateImmutableRules(new ClassUnderTest(), modifiedObject, rules);
        assertEquals(Arrays.asList("error.validation.immutable.classundertest.utilDate"), errors);
    }


    @Test
    public void validateContentRules_nestedIndexed() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.content("subClassArrayProp[*].stringArrayProp[0-2]", Equals.any("b2", "b3", "c2", "c3", "d2", "d3"));
        final List<String> errors = Validator.instance().validateContentRules(new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty());
    }


    @Test
    public void validateUpdateRules_ok() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.update("stringProp", Equals.any("FOO"), Condition.of("stringProp",  Equals.any("BAR", "ZOO")));

        ClassUnderTest modifiedObject = new ClassUnderTest("FOO", null);
        ClassUnderTest originalObject = new ClassUnderTest("ZOO", null);

        final List<String> errors = Validator.instance().validateUpdateRules(originalObject, modifiedObject, rules);
        assertTrue(errors.isEmpty());
    }

    @Test
    public void validateUpdateRules_nok() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.update("stringProp", Equals.any("XYZ"), Condition.of("stringProp",  Equals.any("BAR", "ZOO")));

        ClassUnderTest modifiedObject = new ClassUnderTest("FOO", null);
        ClassUnderTest originalObject = new ClassUnderTest("ZOO", null);

        final List<String> errors = Validator.instance().validateUpdateRules(originalObject, modifiedObject, rules);
        assertEquals(Arrays.asList("error.validation.update.equals_any.classundertest.stringProp"), errors);
    }

    @Test
    public void validateUpdateRules_complexStateTransitions() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        // everyone: ONE -> [TWO, THREE] resp. [TWO, THREE] -> FOUR
        // MANAGER: may set any value
        // EXPERT: additionally FOUR -> ONE
        // Note: the rule for MANAGER could be simplified as content rule w/o Constraint.ref
        rules.update("enumProp", Equals.any(ValidationTesting.SomeEnum.values()),
                Permissions.any("MANAGER"),
                Condition.of("enumProp",  Equals.any(ValidationTesting.SomeEnum.values())));
        rules.update("enumProp", Equals.any("TWO", "THREE"),
                Condition.of("enumProp",  Equals.any("ONE")));
        rules.update("enumProp", Equals.any("FOUR"),
                Condition.of("enumProp",  Equals.any("TWO", "THREE")));
        rules.update("enumProp", Equals.any("FOUR"),
                Condition.of("enumProp",  Equals.any("FOUR")));
        rules.update("enumProp", Equals.any("ONE", "FOUR"),
                Permissions.any("EXPERT"),
                Condition.of("enumProp",  Equals.any("FOUR")));

        ClassUnderTest modifiedObject = new ClassUnderTest(null, SomeEnum.ONE);
        ClassUnderTest originalObject = new ClassUnderTest(null, SomeEnum.FOUR);

        List<String> errors;
        errors = Validator.instance().validateUpdateRules(originalObject, modifiedObject, UserPermissions.of("MANAGER"), rules);
        assertTrue(errors.isEmpty());

        errors = Validator.instance().validateUpdateRules(originalObject, modifiedObject, UserPermissions.of("EXPERT"), rules);
        assertTrue(errors.isEmpty());

        errors = Validator.instance().validateUpdateRules(originalObject, modifiedObject, UserPermissions.of("TRAINEE"), rules);
        assertEquals(Arrays.asList("error.validation.update.equals_any.classundertest.enumProp"), errors);
    }


    class ClassUnderTest extends BaseClass implements Identifiable<Integer> {
        private String stringProp;
        private SomeEnum enumProp;
        private Date utilDate = Date.from(LocalDate.of(2020, 1, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
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

        public Date getUtilDate() {
            return utilDate;
        }

        public void setUtilDate(Date utilDate) {
            this.utilDate = utilDate;
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
        private String stringProp = null;
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
        ONE, TWO, THREE, FOUR
    }

    enum OtherEnum {
        ONE
    }

}

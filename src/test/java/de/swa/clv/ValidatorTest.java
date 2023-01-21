package de.swa.clv;

import de.swa.clv.constraints.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.params.provider.Arguments.arguments;

class ValidatorTest {
    public static final LocalDate A_LOCAL_DATE = LocalDate.of(2022, 1, 1);

    /*
     * Testing validateProperty
     */

    @Test
    void validateProperty_simplePropertyInherited() {
        try {
            Validator.instance().validateProperty("id", ClassUnderTest.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    void validateProperty_simpleProperty() {
        try {
            Validator.instance().validateProperty("stringProp", ClassUnderTest.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    void validateProperty_nestedProperty() {
        try {
            Validator.instance().validateProperty("subClassProp.stringProp", ClassUnderTest.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    void validateProperty_nestedIndexedArrayProperty() {
        try {
            Validator.instance().validateProperty("subClassProp.stringArrayProp[0]", ClassUnderTest.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    void validateProperty_indexedArrayIndexedListProperty() {
        try {
            Validator.instance().validateProperty("subClassArrayProp[0].stringListProp[999]", ClassUnderTest.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    void validateProperty_notexisting() {
        assertThrows(IllegalArgumentException.class,
                () -> Validator.instance().validateProperty("notexisting", ClassUnderTest.class));
    }


    @Test
    void inflateIndexedProperty_starAndIncrement() {
        List<String> expected = Arrays.asList(
                "subClassArrayProp[0].stringArrayProp[2]",
                "subClassArrayProp[0].stringArrayProp[4]",
                "subClassArrayProp[1].stringArrayProp[2]");

        List<String> inflated = Validator.instance().inflateIndexedProperty("subClassArrayProp[*].stringArrayProp[2/2]", new ClassUnderTest());

        assertEquals(expected, inflated);
    }

    @Test
    void inflateIndexedProperty_listAndRange() {
        List<String> expected = Arrays.asList(
                "subClassArrayProp[2].stringListProp[7]",
                "subClassArrayProp[2].stringListProp[8]",
                "subClassArrayProp[2].stringListProp[9]",
                "subClassArrayProp[3].stringListProp[7]",
                "subClassArrayProp[3].stringListProp[8]",
                "subClassArrayProp[3].stringListProp[9]");

        List<String> inflated = Validator.instance().inflateIndexedProperty("subClassArrayProp[2-3].stringListProp[7-9]", new ClassUnderTest());

        assertEquals(expected, inflated);
    }


    private ValidationRules<ClassUnderTest> classUnderTestRules;
    @BeforeEach
    void before() {
        classUnderTestRules = new ValidationRules<>(ClassUnderTest.class);
        classUnderTestRules.mandatory("stringProp", Condition.of("utilDate", Equals.notNull()))
                .errorCodeControl(UseType.AS_SUFFIX, "#1st");
        classUnderTestRules.mandatory("stringProp", Condition.of("enumProp", Equals.notNull()))
                .errorCodeControl(UseType.AS_SUFFIX, "#2nd");
        classUnderTestRules.immutable("enumProp", Condition.of("utilDate", Equals.notNull()))
                .errorCodeControl(UseType.AS_SUFFIX, "#1st");
        classUnderTestRules.immutable("enumProp", Condition.of("stringProp", Equals.null_()))
                .errorCodeControl(UseType.AS_SUFFIX, "#2nd");
    }

    @Test
    void validate_mandatory_errorCode_suffix() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("stringProp")
                .errorCodeControl(UseType.AS_SUFFIX, "#suffix");
        ClassUnderTest object = new ClassUnderTest(null, null);
        List<String> errors = Validator.instance().validateMandatoryRules(object, rules);
        assertEquals(List.of("error.validation.mandatory.classundertest.stringProp#suffix"), errors);
    }

    @Test
    void validate_mandatory_errorCode_replacement() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("stringProp")
                .errorCodeControl(UseType.AS_REPLACEMENT, "errorCode123");
        ClassUnderTest object = new ClassUnderTest(null, null);
        List<String> errors = Validator.instance().validateMandatoryRules(object, rules);
        assertEquals(List.of("errorCode123"), errors);
    }

    @Test
    void validate_mandatory_functionNotAllowed() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> rules.mandatory("subClassArrayProp[*]#distinct", Permissions.any("ONE")));
        assertEquals("Aggregate functions are not allowed for mandatory and immutable property rules: " +
                "subClassArrayProp[*]#distinct", ex.getMessage());
    }

    @Test
    void validate_immmutable_functionNotAllowed() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> rules.immutable("subClassArrayProp[*]#distinct", Permissions.any("ONE")));
        assertEquals("Aggregate functions are not allowed for mandatory and immutable property rules: " +
                "subClassArrayProp[*]#distinct", ex.getMessage());
    }

    @Test
    void validate_permissions_string_vs_string() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("stringProp", Permissions.any("ONE"));
        ClassUnderTest object = new ClassUnderTest(null, null);
        List<String> errors = Validator.instance().validateMandatoryRules(object, UserPermissions.of("ONE"), rules);
        assertEquals(List.of("error.validation.mandatory.classundertest.stringProp"), errors);
    }

    @Test
    void validate_permissions_enum_vs_string() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("stringProp", Permissions.any(SomeEnum.ONE));
        ClassUnderTest object = new ClassUnderTest(null, null);
        List<String> errors = Validator.instance().validateMandatoryRules(object, UserPermissions.of("ONE"), rules);
        assertEquals(List.of("error.validation.mandatory.classundertest.stringProp"), errors);
    }

    @Test
    void validate_permissions_string_vs_enum() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("stringProp", Permissions.any("ONE"));
        ClassUnderTest object = new ClassUnderTest(null, null);
        List<String> errors = Validator.instance().validateMandatoryRules(object, UserPermissions.of(SomeEnum.ONE), rules);
        assertEquals(List.of("error.validation.mandatory.classundertest.stringProp"), errors);
    }

    @Test
    void validate_permissions_enum_vs_enum() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("stringProp", Permissions.any(SomeEnum.ONE));
        ClassUnderTest object = new ClassUnderTest(null, null);
        List<String> errors = Validator.instance().validateMandatoryRules(object, UserPermissions.of(OtherEnum.ONE), rules);
        assertEquals(List.of("error.validation.mandatory.classundertest.stringProp"), errors);
    }

    @Test
    void validateMandatoryRules_true() {
        ClassUnderTest object = new ClassUnderTest("someString", null);
        List<String> errors = Validator.instance().validateMandatoryRules(object, classUnderTestRules);
        assertTrue(errors.isEmpty(), errors.toString());
    }

    @Test
    void validateMandatoryRules_false() {
        ClassUnderTest object = new ClassUnderTest(null, SomeEnum.ONE);
        List<String> errors = Validator.instance().validateMandatoryRules(object, classUnderTestRules);
        assertEquals(List.of(
                "error.validation.mandatory.classundertest.stringProp#1st",
                "error.validation.mandatory.classundertest.stringProp#2nd"), errors);
    }

    @Test
    void validateMandatoryRules_star() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("subClassArrayProp[*].stringProp");
        List<String> errors = Validator.instance().validateMandatoryRules(new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty(), errors.toString());
    }

    @Test
    void validateMandatoryRules_starFail() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.mandatory("subClassArrayProp[*].stringProp");

        ClassUnderTest classUnderTest = new ClassUnderTest();
        classUnderTest.subClassArrayProp[0].stringProp = null;

        List<String> errors = Validator.instance().validateMandatoryRules(classUnderTest, rules);
        assertEquals(List.of("error.validation.mandatory.classundertest.subClassArrayProp[*].stringProp"), errors);
    }


    @Test
    void validateImmutableRules_sameEnums() {
        ClassUnderTest original = new ClassUnderTest("someString", SomeEnum.ONE);
        ClassUnderTest modified1 = new ClassUnderTest("otherString", SomeEnum.ONE);
        List<String> errors = Validator.instance().validateImmutableRules(original, modified1, classUnderTestRules);
        assertTrue(errors.isEmpty(), errors.toString());
    }

    @Test
    void validateImmutableRules_nullVsNull() {
        ClassUnderTest original = new ClassUnderTest("someString", null);
        ClassUnderTest modified1 = new ClassUnderTest("otherString", null);
        List<String> errors = Validator.instance().validateImmutableRules(original, modified1, classUnderTestRules);
        assertTrue(errors.isEmpty(), errors.toString());
    }

    @Test
    void validateImmutableRules_differentEnums() {
        ClassUnderTest original = new ClassUnderTest(null, SomeEnum.ONE);
        ClassUnderTest modified1 = new ClassUnderTest(null, SomeEnum.TWO);
        List<String> errors = Validator.instance().validateImmutableRules(original, modified1, classUnderTestRules);
        assertEquals(List.of(
                "error.validation.immutable.classundertest.enumProp#1st",
                "error.validation.immutable.classundertest.enumProp#2nd"), errors);
    }

    @Test
    void validateImmutableRules_enumVsNull() {
        ClassUnderTest original = new ClassUnderTest("someString", SomeEnum.ONE);
        ClassUnderTest modified1 = new ClassUnderTest("otherString", null);
        List<String> errors = Validator.instance().validateImmutableRules(original, modified1, classUnderTestRules);
        assertEquals(List.of("error.validation.immutable.classundertest.enumProp#1st"), errors);
    }

    @Test
    void validateImmutableRules_nullVsEnum() {
        ClassUnderTest original = new ClassUnderTest("someString", null);
        ClassUnderTest modified1 = new ClassUnderTest("otherString", SomeEnum.TWO);
        List<String> errors = Validator.instance().validateImmutableRules(original, modified1, classUnderTestRules);
        assertEquals(List.of("error.validation.immutable.classundertest.enumProp#1st"), errors);
    }

    @Test
    void validateImmutableRules_star() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.immutable("subClassArrayProp[*].stringProp");
        List<String> errors = Validator.instance().validateImmutableRules(
                new ClassUnderTest(), new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty(), errors.toString());
        assertEquals(1, rules.getImmutableRulesKeys().size());
    }

    @Test
    void validateImmutableRules_starFail() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.immutable("subClassArrayProp[*].stringProp");
        ClassUnderTest modifiedObject = new ClassUnderTest();
        modifiedObject.subClassArrayProp[1].stringProp = "modified";
        List<String> errors = Validator.instance().validateImmutableRules(new ClassUnderTest(), modifiedObject, rules);
        assertEquals(List.of("error.validation.immutable.classundertest.subClassArrayProp[*].stringProp"), errors);
    }

    @Test
    void validateImmutableRules_utilDate() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.immutable("utilDate");
        List<String> errors = Validator.instance().validateImmutableRules(new ClassUnderTest(), new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty(), errors.toString());
        assertEquals(1, rules.getImmutableRulesKeys().size());
    }

    @Test
    void validateImmutableRules_utilDateFail() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.immutable("utilDate");
        ClassUnderTest modifiedObject = new ClassUnderTest();
        modifiedObject.setUtilDate(Date.from(LocalDate.of(2020, 1, 2 )
                .atStartOfDay(ZoneId.systemDefault()).toInstant()));
        List<String> errors = Validator.instance().validateImmutableRules(new ClassUnderTest(), modifiedObject, rules);
        assertEquals(List.of("error.validation.immutable.classundertest.utilDate"), errors);
    }


    @Test
    void validateContentRules_nestedIndexed() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.content("subClassArrayProp[*].stringArrayProp[0-2]", Equals.any("b2", "b3", "c2", "c3", "d2", "d3"));
        List<String> errors = Validator.instance().validateContentRules(new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty(), errors.toString());
        assertEquals(1, rules.getContentRulesKeys().size());
    }

    @Test
    void validateContentRules_sum_integer_Range() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.content("subClassArrayProp[*].integerListProp[0-2]#sum", Range.minMax(1, 12));
        List<String> errors = Validator.instance().validateContentRules(new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty(), errors.toString());
        assertEquals(1, rules.getContentRulesKeys().size());
    }

    @Test
    void validateContentRules_sum_float_Range() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.content("floatArray[*]#sum", Range.minMax(6.66f, 6.66f));
        List<String> errors = Validator.instance().validateContentRules(new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty(), errors.toString());
        assertEquals(1, rules.getContentRulesKeys().size());
    }

    @Test
    void validateContentRules_integer_sum_RegEx() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.content("subClassArrayProp[*].integerListProp[0-2]#sum", RegEx.any("^[0-9]+$"));
        List<String> errors = Validator.instance().validateContentRules(new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty(), errors.toString());
    }

    @Test
    void validateContentRules_integer_sum_EqualsAny() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.content("subClassArrayProp[*].integerListProp[0-2]#sum", Equals.any(12));
        List<String> errors = Validator.instance().validateContentRules(new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty(), errors.toString());
        assertEquals(1, rules.getContentRulesKeys().size());
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "subClassArrayProp[0].integerListProp[*]#distinct",
            "emptyStringArray[*]#distinct",
            "nullStringArray[*]#distinct"})
    void validateContentRules_distinct_EqualsAny_true(String propertyWithTerminalFunctionDistinct) {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.content(propertyWithTerminalFunctionDistinct, Equals.any(true));
        List<String> errors = Validator.instance().validateContentRules(new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty(), errors.toString());
        assertEquals(1, rules.getContentRulesKeys().size());
    }

    @Test
    void validateContentRules_sumEverywhere() {
        ConditionConstraint condition = Condition.of("subClassArrayProp[*].integerListProp[*]#sum",
                Equals.any(12));
        // this assures, that Equals.anyRef below is validated at all!
        ClassUnderTest entity = new ClassUnderTest();
        assertTrue(Validator.instance().conditionIsMet(condition, entity, entity));

        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.content("subClassArrayProp[0].integerListProp[*]#sum",
                Equals.anyRef("subClassArrayProp[1].integerListProp[*]#sum"),
                condition);
        List<String> errors = Validator.instance().validateContentRules(entity, rules);
        assertTrue(errors.isEmpty(), errors.toString());
        assertEquals(1, rules.getContentRulesKeys().size());
    }

    @Test
    void validateContentRules_distinctEverywhere() {
        ConditionConstraint condition = Condition.of("subClassArrayProp[0].integerListProp[*]#distinct",
                Equals.any(true));
        // this assures, that Equals.anyRef below is validated at all!
        ClassUnderTest entity = new ClassUnderTest();
        assertTrue(Validator.instance().conditionIsMet(condition, entity, entity));

        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.content("subClassArrayProp[0].integerListProp[*]#distinct",
                Equals.anyRef("subClassArrayProp[1].integerListProp[*]#distinct"),
                condition);
        List<String> errors = Validator.instance().validateContentRules(entity, rules);
        assertTrue(errors.isEmpty(), errors.toString());
        assertEquals(1, rules.getContentRulesKeys().size());
    }

    @Test
    void validateUpdateRules_ok() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.update("stringProp", Equals.any("FOO"), Condition.of("stringProp",
                Equals.any("BAR", "ZOO")));

        ClassUnderTest modifiedObject = new ClassUnderTest("FOO", null);
        ClassUnderTest originalObject = new ClassUnderTest("ZOO", null);

        List<String> errors = Validator.instance().validateUpdateRules(originalObject, modifiedObject, rules);
        assertTrue(errors.isEmpty(), errors.toString());
    }

    @Test
    void validateUpdateRules_nok() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.update("stringProp", Equals.any("XYZ"), Condition.of("stringProp",
                Equals.any("BAR", "ZOO")));

        ClassUnderTest modifiedObject = new ClassUnderTest("FOO", null);
        ClassUnderTest originalObject = new ClassUnderTest("ZOO", null);

        List<String> errors = Validator.instance().validateUpdateRules(originalObject, modifiedObject, rules);
        assertEquals(Arrays.asList("error.validation.update.equals_any.classundertest.stringProp"), errors);
    }

    static Stream<Arguments> complexStateTransitionsValueProvider() {
        ClassUnderTest originalOne = new ClassUnderTest(null, SomeEnum.ONE);
        ClassUnderTest originalTwo = new ClassUnderTest(null, SomeEnum.TWO);
        ClassUnderTest originalThree = new ClassUnderTest(null, SomeEnum.THREE);
        ClassUnderTest originalFour = new ClassUnderTest(null, SomeEnum.FOUR);
        ClassUnderTest modifiedOne = new ClassUnderTest(null, SomeEnum.ONE);
        ClassUnderTest modifiedTwo = new ClassUnderTest(null, SomeEnum.TWO);
        ClassUnderTest modifiedThree = new ClassUnderTest(null, SomeEnum.THREE);
        ClassUnderTest modifiedFour = new ClassUnderTest(null, SomeEnum.FOUR);
        UserPermissions trainee = UserPermissions.of("TRAINEE");
        UserPermissions expert = UserPermissions.of("EXPERT");
        UserPermissions manager = UserPermissions.of("MANAGER");
        String errorPrefix = "error.validation.update.equals_any.classundertest.enumProp";
        return Stream.of(
                arguments(originalOne, modifiedTwo, trainee, List.of()),
                arguments(originalOne, modifiedThree, trainee, List.of()),
                arguments(originalOne, modifiedFour, trainee, List.of(errorPrefix + "#1")),
                arguments(originalTwo, modifiedFour, trainee, List.of()),
                arguments(originalThree, modifiedFour, trainee, List.of()),
                arguments(originalTwo, modifiedOne, trainee, List.of(errorPrefix + "#2")),
                arguments(originalFour, modifiedOne, trainee, List.of(errorPrefix + "#3")),

                arguments(originalOne, modifiedTwo, expert, List.of()),
                arguments(originalOne, modifiedThree, expert, List.of()),
                arguments(originalOne, modifiedFour, expert, List.of(errorPrefix + "#1")),
                arguments(originalTwo, modifiedFour, expert, List.of()),
                arguments(originalThree, modifiedFour, expert, List.of()),
                arguments(originalTwo, modifiedOne, expert, List.of(errorPrefix + "#2")),
                arguments(originalFour, modifiedOne, expert, List.of()),

                arguments(originalOne, modifiedTwo, manager, List.of()),
                arguments(originalOne, modifiedThree, manager, List.of()),
                arguments(originalOne, modifiedFour, manager, List.of()),
                arguments(originalTwo, modifiedFour, manager, List.of()),
                arguments(originalThree, modifiedFour, manager, List.of()),
                arguments(originalTwo, modifiedOne, manager, List.of()),
                arguments(originalFour, modifiedOne, manager, List.of()));
    }

    @ParameterizedTest
    @MethodSource("complexStateTransitionsValueProvider")
    void validateUpdateRules_complexStateTransitions(ClassUnderTest original, ClassUnderTest modified,
            UserPermissions userPermissions, List<String> expectedErrors) {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        // everyone: ONE -> [TWO, THREE] resp. [TWO, THREE] -> FOUR
        // EXPERT: additionally FOUR -> ONE
        // MANAGER: may set any value
        // Note: the rule for MANAGER could be simplified as content(!) rule w/o Condition.of
        rules.update("enumProp", Equals.any("TWO", "THREE"),
                        Permissions.none("MANAGER"),
                        Condition.of("enumProp",  Equals.any("ONE")))
                .errorCodeControl(UseType.AS_SUFFIX, "#1");
        rules.update("enumProp", Equals.any("FOUR"),
                        Permissions.none("MANAGER"),
                        Condition.of("enumProp",  Equals.any("TWO", "THREE")))
                .errorCodeControl(UseType.AS_SUFFIX, "#2");
        rules.update("enumProp", Equals.any("FOUR"),
                        Permissions.none("MANAGER", "EXPERT"),
                        Condition.of("enumProp",  Equals.any("FOUR")))
                .errorCodeControl(UseType.AS_SUFFIX, "#3");
        rules.update("enumProp", Equals.any("ONE"),
                        Permissions.any("EXPERT"),
                        Condition.of("enumProp",  Equals.any("FOUR")))
                .errorCodeControl(UseType.AS_SUFFIX, "#4");
        rules.update("enumProp", Equals.any(ValidationTesting.SomeEnum.values()),
                        Permissions.any("MANAGER"),
                        Condition.of("enumProp",  Equals.any(ValidationTesting.SomeEnum.values())))
                .errorCodeControl(UseType.AS_SUFFIX, "#5");

        List<String> errors;
        errors = Validator.instance().validateUpdateRules(original, modified, userPermissions, rules);
        assertEquals(expectedErrors, errors);
    }

    @Test
    void aggregateFunction_propertyNotIndexed() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Validator.instance().validateAndGetTerminalAggregateFunctionIfExist("foo.bar#sum"));
        assertEquals("Aggregate functions are only allowed for indexed properties: foo.bar#sum",
                ex.getMessage());
    }

    @Test
    void aggregateFunction_tooManyMarkers() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Validator.instance().validateAndGetTerminalAggregateFunctionIfExist("foo[*].#bar#sum"));
        assertEquals("Property must not contain more then one aggregate function markers (#): foo[*].#bar#sum",
                ex.getMessage());
    }

    @Test
    void aggregateFunction_unknown() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> Validator.instance().validateAndGetTerminalAggregateFunctionIfExist("foo[*]#avg"));
        assertEquals("Property contains unknown aggregate function: foo[*]#avg",
                ex.getMessage());
    }

    @Test
    void validateRecord_ok() {
        ValidationRules<Record> rules = new ValidationRules<>(Record.class);
        rules.mandatory("aString", Condition.of("aString", Equals.anyRef("aString")));
        rules.immutable("aLong", Condition.of("aString", Equals.anyRef("aString")));
        rules.content("aLocalDate", Quarter.anyRef("aLong"));

        Record record1 = new Record("foo", 1, A_LOCAL_DATE);
        Record record2 = new Record("foo", 1, A_LOCAL_DATE);
        List<String> errors = Validator.instance().validateMandatoryRules(record1, rules);
        errors.addAll(Validator.instance().validateImmutableRules(record1, record2, rules));
        errors.addAll(Validator.instance().validateContentRules(record1, rules));

        assertEquals(0, errors.size());
    }

    @Test
    void validateRecord_2errors() {
        ValidationRules<Record> rules = new ValidationRules<>(Record.class);
        rules.mandatory("aString");
        rules.immutable("aLong", Condition.of("aString", Equals.null_()));

        Record record1 = new Record(null, 1, A_LOCAL_DATE);
        List<String> errors = Validator.instance().validateMandatoryRules(record1, rules);
        errors.addAll(Validator.instance().validateContentRules(record1, rules));
        Record record2 = new Record(null, 2, A_LOCAL_DATE);
        errors.addAll(Validator.instance().validateImmutableRules(record1, record2, rules));

        assertEquals(List.of("error.validation.mandatory.record.aString", "error.validation.immutable.record.aLong"),
                errors);
    }


    @Test
    void validate_content_LocalDate_QuarterAny() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.content("localDateNewYear", Quarter.any(3, 1));
        List<String> errors = Validator.instance().validateContentRules(new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty(), errors.toString());
    }

    @Test
    void validate_content_LocalDate_QuarterAny_fail() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.content("localDateNewYear", Quarter.any(2, 4));
        List<String> errors = Validator.instance().validateContentRules(new ClassUnderTest(), rules);
        assertEquals(List.of("error.validation.content.quarter_any.classundertest.localDateNewYear"), errors);
    }

    @Test
    void validate_content_LocalDate_QuarterAnyRef() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.content("localDateNewYear", Quarter.anyRef("id", "subClassProp.integerListProp[*]"));
        List<String> errors = Validator.instance().validateContentRules(new ClassUnderTest(), rules);
        assertTrue(errors.isEmpty(), errors.toString());
    }

    @Test
    void validate_content_LocalDate_QuarterAnyRef_fail() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.content("localDateNewYear", Quarter.anyRef("subClassProp.integerListProp[1/1]"));
        List<String> errors = Validator.instance().validateContentRules(new ClassUnderTest(), rules);
        assertEquals(List.of("error.validation.content.quarter_any_ref.classundertest.localDateNewYear"), errors);
    }

    /*
     * Testing ofUpdate() and ofCurrent()
     */
    
    @Test
    void mandatoryRuleConditionConstraint_ofUpdate() {
        ValidationRules<Record> rules = new ValidationRules<>(Record.class);
        assertThrows(IllegalArgumentException.class,
            () -> rules.mandatory("aString",
                    Condition.of("aString", Equals.anyRef("aString").ofUpdate())));
    }

    @Test
    void mandatoryRuleConditionConstraint_ofCurrent() {
        ValidationRules<Record> rules = new ValidationRules<>(Record.class);
        assertThrows(IllegalArgumentException.class,
            () -> rules.mandatory("aString",
                    Condition.of("aString", Equals.anyRef("aString").ofCurrent())));
    }

    @Test
    void contentRulePropertyConstraint_ofUpdate() {
        ValidationRules<Record> rules = new ValidationRules<>(Record.class);
        assertThrows(IllegalArgumentException.class,
            () -> rules.content("aString", 
                    Equals.anyRef("aString").ofUpdate(),
                    Condition.of("aString", Equals.anyRef("aString"))));
    }

    @Test
    void contentRulePropertyConstraint_ofCurrent() {
        ValidationRules<Record> rules = new ValidationRules<>(Record.class);
        assertThrows(IllegalArgumentException.class,
            () -> rules.content("aString", 
                    Equals.anyRef("aString").ofCurrent(),
                    Condition.of("aString", Equals.anyRef("aString"))));
    }

    @Test
    void contentRuleConditionConstraint_ofUpdate() {
        ValidationRules<Record> rules = new ValidationRules<>(Record.class);
        assertThrows(IllegalArgumentException.class,
            () -> rules.content("aString", 
                    Equals.anyRef("aString"),
                    Condition.of("aString", Equals.anyRef("aString").ofUpdate())));
    }

    @Test
    void contentRuleConditionConstraint_ofCurrent() {
        ValidationRules<Record> rules = new ValidationRules<>(Record.class);
        assertThrows(IllegalArgumentException.class,
            () -> rules.content("aString", 
                    Equals.anyRef("aString"),
                    Condition.of("aString", Equals.anyRef("aString").ofCurrent())));
    }

    @Test
    void immutableRuleConditionConstraint_ofUpdate() {
        ValidationRules<Record> rules = new ValidationRules<>(Record.class);
        rules.immutable("aString", 
                Condition.of("aString", Equals.anyRef("aString").ofUpdate()));
        assertTrue(true);
    }

    @Test
    void immutableRuleConditionConstraint_ofCurrent() {
        ValidationRules<Record> rules = new ValidationRules<>(Record.class);
        assertThrows(IllegalArgumentException.class,
            () -> rules.immutable("aString", 
                    Condition.of("aString", Equals.anyRef("aString").ofCurrent())));
    }

    @Test
    void updateRulePropertyConstraint_ofUpdate() {
        ValidationRules<Record> rules = new ValidationRules<>(Record.class);
        assertThrows(IllegalArgumentException.class,
                () -> rules.update("aString",
                        Equals.anyRef("aString").ofUpdate(),
                        Condition.of("aString", Equals.anyRef("aString"))));
    }

    @Test
    void updateRulePropertyConstraint_ofCurrent() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.update("stringProp", 
                Equals.anyRef("stringProp").ofCurrent(),
                Condition.of("enumProp", Equals.null_())); //TODO ConditionContraints doch optional!

        ClassUnderTest modifiedObject = new ClassUnderTest("FOO", null);
        ClassUnderTest originalObject = new ClassUnderTest("ZOO", null);

        List<String> errors = Validator.instance().validateUpdateRules(originalObject, modifiedObject, rules);
        assertEquals(List.of("error.validation.update.equals_any_ref.classundertest.stringProp"), errors);
    }

    @Test
    void updateRuleConditionConstraint_ofUpdate() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        rules.update("stringProp",
                Equals.any("BAR"),
                Condition.of("stringProp", Equals.noneRef("stringProp").ofUpdate()));

        ClassUnderTest editedEntity = new ClassUnderTest("FOO", null);
        ClassUnderTest currentEntity = new ClassUnderTest("ZOO", null);

        List<String> errors = Validator.instance().validateUpdateRules(currentEntity, editedEntity, rules);
        assertEquals(List.of("error.validation.update.equals_any.classundertest.stringProp"), errors);
    }

    @Test
    void updateRuleConditionConstraint_ofCurrent() {
        ValidationRules<Record> rules = new ValidationRules<>(Record.class);
        assertThrows(IllegalArgumentException.class,
                () -> rules.update("aString", Equals.anyRef("aString"),
                        Condition.of("aString", Equals.anyRef("aString").ofCurrent())));
    }


    @Test
    void validateValueComparerConstraint_sumUnchangedTrue() {
        ClassUnderTest thisEntity = new ClassUnderTest();
        thisEntity.setFloatArray(new float[] {1.11f, 2.22f, 3.33f});
        ClassUnderTest thatEntity = new ClassUnderTest();
        thatEntity.setFloatArray(new float[] {4.44f, 2.22f});
        boolean valid = Validator.instance().validateValueComparerConstraint("floatArray[*]",
                AggregateFunction.sum, Value.unchanged(), thisEntity, thatEntity);
        assertTrue(valid);
    }

    @Test
    void validateValueComparerConstraint_sumUnchangedFalse() {
        ClassUnderTest thisEntity = new ClassUnderTest();
        thisEntity.setFloatArray(new float[] {1.11f, 2.22f, 3.33f});
        ClassUnderTest thatEntity = new ClassUnderTest();
        thatEntity.setFloatArray(new float[] {1.11f, 2.22f, 3.34f});
        boolean valid = Validator.instance().validateValueComparerConstraint("floatArray[*]",
                AggregateFunction.sum, Value.unchanged(), thisEntity, thatEntity);
        assertFalse(valid);
    }

    @Test
    void validateValueComparerConstraint_sumChangedTrue() {
        ClassUnderTest thisEntity = new ClassUnderTest();
        thisEntity.setFloatArray(new float[] {1.11f, 2.22f, 3.33f});
        ClassUnderTest thatEntity = new ClassUnderTest();
        thatEntity.setFloatArray(new float[] {1.11f, 2.22f, 3.34f});
        boolean valid = Validator.instance().validateValueComparerConstraint("floatArray[*]",
                AggregateFunction.sum, Value.changed(), thisEntity, thatEntity);
        assertTrue(valid);
    }

    @Test
    void validateValueComparerConstraint_sumChangedFalse() {
        ClassUnderTest thisEntity = new ClassUnderTest();
        thisEntity.setFloatArray(new float[] {1.11f, 2.22f, 3.33f});
        ClassUnderTest thatEntity = new ClassUnderTest();
        thatEntity.setFloatArray(new float[] {4.44f, 2.22f});
        boolean valid = Validator.instance().validateValueComparerConstraint("floatArray[*]",
                AggregateFunction.sum, Value.changed(), thisEntity, thatEntity);
        assertFalse(valid);
    }

    @Test
    void validateValueComparerConstraint_distinctUnchangedTrue() {
        ClassUnderTest thisEntity = new ClassUnderTest();
        thisEntity.setFloatArray(new float[] {1.11f, 2.22f, 3.33f}); // distinct = true
        ClassUnderTest thatEntity = new ClassUnderTest();
        thatEntity.setFloatArray(new float[] {3.33f, 2.22f}); // distinct = true
        boolean valid = Validator.instance().validateValueComparerConstraint("floatArray[*]",
                AggregateFunction.distinct, Value.unchanged(), thisEntity, thatEntity);
        assertTrue(valid);
    }

    @Test
    void validateValueComparerConstraint_distinctUnchangedFalse() {
        ClassUnderTest thisEntity = new ClassUnderTest();
        thisEntity.setFloatArray(new float[] {1.11f, 2.22f, 3.33f}); // distinct = true
        ClassUnderTest thatEntity = new ClassUnderTest();
        thatEntity.setFloatArray(new float[] {2.22f, 2.22f}); // distinct = false
        boolean valid = Validator.instance().validateValueComparerConstraint("floatArray[*]",
                AggregateFunction.distinct, Value.unchanged(), thisEntity, thatEntity);
        assertFalse(valid);
    }

    @Test
    void validateValueComparerConstraint_distinctChangedTrue() {
        ClassUnderTest thisEntity = new ClassUnderTest();
        thisEntity.setFloatArray(new float[] {1.11f, 1.11f}); // distinct = false
        ClassUnderTest thatEntity = new ClassUnderTest();
        thatEntity.setFloatArray(new float[] {1.11f, 2.22f}); // distinct = true
        boolean valid = Validator.instance().validateValueComparerConstraint("floatArray[*]",
                AggregateFunction.distinct, Value.changed(), thisEntity, thatEntity);
        assertTrue(valid);
    }

    @Test
    void validateValueComparerConstraint_distinctChangedFalse() {
        ClassUnderTest thisEntity = new ClassUnderTest();
        thisEntity.setFloatArray(new float[] {1.11f, 2.22f}); // distinct = true
        ClassUnderTest thatEntity = new ClassUnderTest();
        thatEntity.setFloatArray(new float[] {2.22f, 3.33f}); // distinct = true
        boolean valid = Validator.instance().validateValueComparerConstraint("floatArray[*]",
                AggregateFunction.distinct, Value.changed(), thisEntity, thatEntity);
        assertFalse(valid);
    }

    @Test
    void validateValueComparerConstraint_unchangedTrue() {
        ClassUnderTest thisEntity = new ClassUnderTest();
        thisEntity.setFloatArray(new float[] {1.11f, 2.22f});
        ClassUnderTest thatEntity = new ClassUnderTest();
        thatEntity.setFloatArray(new float[] {1.11f, 2.22f});
        boolean valid = Validator.instance().validateValueComparerConstraint("floatArray[*]",
                null, Value.unchanged(), thisEntity, thatEntity);
        assertTrue(valid);
    }

    @Test
    void validateValueComparerConstraint_unchangedFalse() {
        ClassUnderTest thisEntity = new ClassUnderTest();
        thisEntity.setFloatArray(new float[] {1.11f, 2.22f});
        ClassUnderTest thatEntity = new ClassUnderTest();
        thatEntity.setFloatArray(new float[] {1.11f});
        boolean valid = Validator.instance().validateValueComparerConstraint("floatArray[*]",
                null, Value.unchanged(), thisEntity, thatEntity);
        assertFalse(valid);
    }

    @Test
    void validateValueComparerConstraint_changedTrue() {
        ClassUnderTest thisEntity = new ClassUnderTest();
        thisEntity.setFloatArray(new float[] {1.11f, 2.22f});
        ClassUnderTest thatEntity = new ClassUnderTest();
        thatEntity.setFloatArray(new float[] {1.11f, 2.22f});
        boolean valid = Validator.instance().validateValueComparerConstraint("floatArray[*]",
                null, Value.unchanged(), thisEntity, thatEntity);
        assertTrue(valid);
    }

    @Test
    void validateValueComparerConstraint_changedFalse() {
        ClassUnderTest thisEntity = new ClassUnderTest();
        thisEntity.setFloatArray(new float[] {1.11f, 2.22f});
        ClassUnderTest thatEntity = new ClassUnderTest();
        thatEntity.setFloatArray(new float[] {2.22f, 1.11f});
        boolean valid = Validator.instance().validateValueComparerConstraint("floatArray[*]",
                null, Value.unchanged(), thisEntity, thatEntity);
        assertFalse(valid);
    }

    @Test
    void validateValueComparerConstraint_unchangedArrayTrue() {
        ClassUnderTest thisEntity = new ClassUnderTest();
        thisEntity.setFloatArray(new float[] {1.11f, 2.22f});
        ClassUnderTest thatEntity = new ClassUnderTest();
        thatEntity.setFloatArray(new float[] {1.11f, 2.22f});
        // Validating caches the getter method!
        Validator.instance().validateProperty("floatArray", ClassUnderTest.class);

        boolean valid = Validator.instance().validateValueComparerConstraint("floatArray",
                null, Value.unchanged(), thisEntity, thatEntity);
        assertTrue(valid);
    }


    record Record(String aString, long aLong, LocalDate aLocalDate) {
    }

    static class ClassUnderTest extends BaseClass implements Identifiable<Integer> {
        private String stringProp;
        private SomeEnum enumProp;
        private Date utilDate = Date.from(LocalDate.of(2020, 1, 1)
                .atStartOfDay(ZoneId.systemDefault()).toInstant());
        private LocalDate localDateNewYear = A_LOCAL_DATE;
        private final SubClass subClassProp = new SubClass("a1", new String[] { "b1", "c1" },
                Arrays.asList("d1", "e1", "f1"), Arrays.asList(1, 2, 3));
        private final SubClass[] subClassArrayProp = {
                new SubClass("a2", new String[] { "b2", "c2", "d2", "e2", "f2" }, Arrays.asList("g2", "h2"),
                        Arrays.asList(1, 2, 3)),
                new SubClass("a3", new String[] { "b3", "c3", "d3" }, Arrays.asList("e3", "f3", "g3", "h3"),
                        Arrays.asList(1, 2, 3)) };
        private float[] floatArray = new float[] {1.11f, 2.22f, 3.33f};
        private String[] emptyStringArray = new String[] {};
        private String[] nullStringArray = null;

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

        void setUtilDate(Date utilDate) {
            this.utilDate = utilDate;
        }

        public LocalDate getLocalDateNewYear() {
            return localDateNewYear;
        }

        public float[] getFloatArray() {
            return floatArray;
        }

        void setFloatArray(float[] floatArray) {
            this.floatArray = floatArray;
        }

        public String[] getEmptyStringArray() {
            return emptyStringArray;
        }

        public String[] getNullStringArray() {
            return nullStringArray;
        }
    }

    static class SubClass {
        private String stringProp;
        private final String[] stringArrayProp;
        private final List<String> stringListProp;
        private final List<Integer> integerListProp;


        public SubClass(String stringProp, String[] stringArrayProp, List<String> stringListProp,
                List<Integer> integerListProp) {
            this.stringProp = stringProp;
            this.stringArrayProp = stringArrayProp;
            this.stringListProp = stringListProp;
            this.integerListProp = integerListProp;
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

        public List<Integer> getIntegerListProp() {
            return integerListProp;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            SubClass subClass = (SubClass) o;
            return Objects.equals(stringProp, subClass.stringProp) &&
                    Arrays.equals(stringArrayProp, subClass.stringArrayProp) &&
                    Objects.equals(integerListProp, subClass.integerListProp) &&
                    Objects.equals(stringListProp, subClass.stringListProp);
        }

        @Override
        public int hashCode() {
            int result = Objects.hash(stringProp, stringListProp, integerListProp);
            result = 31 * result + Arrays.hashCode(stringArrayProp);
            return result;
        }
    }

    static class BaseClass {
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

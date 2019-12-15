package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.test.Util;
import org.junit.Test;

import static org.junit.Assert.*;

public class RangeStringTest {

    @Test
    public void validateDefaultComparisonTypeMin() {
        RangeStringAny any = Range.min("Foo");
        assertTrue(any.validate("faa", null));
    }

    @Test
    public void validateDefaultComparisonTypeMax() {
        RangeStringAny any = Range.max("faa");
        assertTrue(any.validate("Foo", null));
    }

    @Test
    public void validateComparisonTypeLexicographicalUnicodeMinAny() {
        RangeRoot any = Range.minAny("Foo", "Fxx").use(ComparisonType.LEXICOGRAPHICAL_UNICODE);
        assertTrue(any.validate("faa", null));
    }

    @Test
    public void validateComparisonTypeLexicographicalUnicodeIgnoringCaseMin() {
        RangeRoot any = Range.min("Foo").use(ComparisonType.LEXICOGRAPHICAL_UNICODE_IGNORING_CASE);
        assertFalse(any.validate("faa", null));
    }

    @Test
    public void serializeToJson() {
        RangeStringAny range = Range.min("123").max("1234567890");
        assertEquals(Util.doubleQuote("'type':'RANGE_ANY','minValues':['123'],'maxValues':['1234567890'],'comparisonType':'LEXICOGRAPHICAL_UNICODE'"), range.serializeToJson());
    }
}
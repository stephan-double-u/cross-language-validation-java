package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.constraints.LessThanString.ComparisonType;
import org.junit.Test;

import static org.junit.Assert.*;

public class LessThanStringTest {

    @Test
    public void validateDefaultComparisonType() {
        LessThanString any = LessThan.lessThan("faa");
        assertTrue(any.validate("Foo", null));
    }

    @Test
    public void validateComparisonTypeLexicographicalUnicodeIgnoringCase() {
        LessThanString any = LessThan.lessThan("faa", ComparisonType.LEXICOGRAPHICAL_UNICODE_IGNORING_CASE);
        assertFalse(any.validate("Foo", null));
    }

}
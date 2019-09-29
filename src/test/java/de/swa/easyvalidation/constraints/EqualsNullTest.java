package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.test.Util;
import org.junit.Test;

import java.sql.JDBCType;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class EqualsNullTest {

    @Test
    public void serializeToJson() {
        EqualsNull null​_ = Equals.null_();
        assertEquals(Util.doubleQuote("'type':'EQUALS_NULL'"), null​_.serializeToJson());
    }

}
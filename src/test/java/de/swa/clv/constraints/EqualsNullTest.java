package de.swa.clv.constraints;

import de.swa.clv.test.Util;
import org.junit.Test;

import java.sql.Timestamp;
import java.time.LocalDate;
import java.util.Date;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class EqualsNullTest {

    @Test
    public void serializeToJson() {
        EqualsNull null_ = Equals.null_();
        assertEquals(Util.doubleQuote("'type':'EQUALS_NULL'"), null_.serializeToJson());
    }

}
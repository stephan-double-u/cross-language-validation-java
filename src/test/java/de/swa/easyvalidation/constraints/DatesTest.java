package de.swa.easyvalidation.constraints;

import org.junit.Test;

import java.time.LocalDate;
import java.util.Calendar;
import java.util.Date;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class DatesTest {

    // TODO more and better tests ...

    @Test
    public void tests() {
        Dates future = Dates.future(1);
        assertEquals("{\"type\":\"DATE\",\"subType\":\"FUTURE\",\"days\":1}", future.serializeToJson());

        assertTrue(future.validate(null, null));

        assertTrue(future.validate(LocalDate.now().plusDays(2), null));

        assertTrue(future.validate(new Date(new Date().getTime() + 24 * 60 * 60 * 1000 + 1000), null)); // Now+1day+1sec

        Calendar futureCalDate = Calendar.getInstance();
        futureCalDate.set(2999, 12, 31);
        assertTrue(future.validate(futureCalDate, null));
    }
}

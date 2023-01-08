package de.swa.clv.constraints;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;

import static de.swa.clv.json.JsonUtil.*;

public abstract class Year extends Dates {

    public static YearAny any(Integer... years) {
        assertValuesAndSizeOk(years);
        return new YearAny(false, years);
    }

    public static YearAny anyOrNull(Integer ... years) {
        assertValuesAndSizeOk(years);
        return new YearAny(true, years);
    }

    public static YearAnyRef anyRef(String ... properties) {
        assertValuesAndSizeOk(properties);
        return new YearAnyRef(false, properties);
    }

    public static YearAnyRef anyRefOrNull(String ... properties) {
        assertValuesAndSizeOk(properties);
        return new YearAnyRef(true, properties);
    }

    @Override
    public String serializeToJson() {
        return serializeToJsonAsValuesArray();
    }

}

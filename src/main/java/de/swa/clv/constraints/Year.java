package de.swa.clv.constraints;

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

}

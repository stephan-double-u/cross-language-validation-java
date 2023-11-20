package de.swa.clv.constraints;

public abstract class Year extends Dates {

    public static YearAny any(Integer... years) {
        assertValuesAndSizeOk(years);
        return new YearAny(false, years);
    }

    public static YearAny anyOrNull(Integer... years) {
        assertValuesAndSizeOk(years);
        return new YearAny(true, years);
    }

    public static YearAnyRef anyRef(String... properties) {
        assertValuesAndSizeOk(properties);
        return new YearAnyRef(false, properties);
    }

    public static YearAnyRef anyRefOrNull(String ... properties) {
        assertValuesAndSizeOk(properties);
        return new YearAnyRef(true, properties);
    }

    /**
     * The year of the date to be validated must be in the relative range specified by the arguments {@code min} and
     * {@code max}, with respect to the current year defined by {@code java.time.Year.now()}.<br>
     * If the date to be validated is {@code null}, the constraint evaluates to {@code false}.
     *
     * @param min the lower bound of the relative range
     * @param max the upper bound of the relative range
     * @return a {@code YearRange} constraint object
     */
    public static YearRange minMaxRelative(int min, int max) {
        return new YearRange(false, min, max, YearRange.RangeType.RELATIVE);
    }

    /**
     * The year of the date to be validated must be in the relative range specified by the arguments {@code min} and
     * {@code max}, with respect to the current year defined by {@code java.time.Year.now()}.<br>
     * If the date to be validated is {@code null}, the constraint evaluates to {@code true}.
     *
     * @param min the lower bound of the relative range
     * @param max the upper bound of the relative range
     * @return a {@code YearRange} constraint object
     */
    public static YearRange minMaxRelativeOrNull(int min, int max) {
        return new YearRange(true, min, max, YearRange.RangeType.RELATIVE);
    }

    /**
     * The year of the date to be checked must be greater than or equal to the year resulting from the current year plus
     * the argument {@code min}.<br>
     * If the date to be validated is {@code null}, the constraint evaluates to {@code false}.
     *
     * @param min the lower bound of the relative range
     * @return a {@code YearRange} constraint object
     */
    public static YearRange minRelative(int min) {
        return new YearRange(false, min, null, YearRange.RangeType.RELATIVE);
    }

    /**
     * The year of the date to be checked must be greater than or equal to the year resulting from the current year plus
     * the argument {@code min}.<br>
     * If the date to be validated is {@code null}, the constraint evaluates to {@code true}.
     *
     * @param min the lower bound of the relative range
     * @return a {@code YearRange} constraint object
     */
    public static YearRange minRelativeOrNull(int min) {
        return new YearRange(true, min, null, YearRange.RangeType.RELATIVE);
    }

    /**
     * The year of the date to be checked must be smaller than or equal to the year resulting from the current year plus
     * the argument {@code max}.<br>
     * If the date to be validated is {@code null}, the constraint evaluates to {@code false}.
     *
     * @param max the upper bound of the relative range
     * @return a {@code YearRange} constraint object
     */
    public static YearRange maxRelative(int max) {
        return new YearRange(false, null, max, YearRange.RangeType.RELATIVE);
    }

    /**
     * The year of the date to be checked must be smaller than or equal to the year resulting from the current year plus
     * the argument {@code max}.<br>
     * If the date to be validated is {@code null}, the constraint evaluates to {@code true}.
     *
     * @param max the upper bound of the relative range
     * @return a {@code YearRange} constraint object
     */
    public static YearRange maxRelativeOrNull(int max) {
        return new YearRange(true, null, max, YearRange.RangeType.RELATIVE);
    }

    /**
     * The year of the date to be validated must be in the absolute range specified by the arguments {@code min} and
     * {@code max}.<br>
     * If the date to be validated is {@code null}, the constraint evaluates to {@code false}.
     *
     * @param min the lower bound of the absolute range
     * @param max the upper bound of the absolute range
     * @return a {@code YearRange} constraint object
     */
    public static YearRange minMaxAbsolute(int min, int max) {
        return new YearRange(false, min, max, YearRange.RangeType.ABSOLUTE);
    }

    /**
     * The year of the date to be validated must be in the absolute range specified by the arguments {@code min} and
     * {@code max}.<br>
     * If the date to be validated is {@code null}, the constraint evaluates to {@code true}.
     *
     * @param min the lower bound of the absolute range
     * @param max the upper bound of the absolute range
     * @return a {@code YearRange} constraint object
     */
    public static YearRange minMaxAbsoluteOrNull(int min, int max) {
        return new YearRange(true, min, max, YearRange.RangeType.ABSOLUTE);
    }

    /**
     * The year of the date to be checked must be greater than or equal to the argument {@code min}.<br>
     * If the date to be validated is {@code null}, the constraint evaluates to {@code false}.
     *
     * @param min the lower bound of the absolute range
     * @return a {@code YearRange} constraint object
     */
    public static YearRange minAbsolute(int min) {
        return new YearRange(false, min, null, YearRange.RangeType.ABSOLUTE);
    }

    /**
     * The year of the date to be checked must be greater than or equal to the argument {@code min}.<br>
     * If the date to be validated is {@code null}, the constraint evaluates to {@code true}.
     *
     * @param min the lower bound of the absolute range
     * @return a {@code YearRange} constraint object
     */
    public static YearRange minAbsoluteOrNull(int min) {
        return new YearRange(true, min, null, YearRange.RangeType.ABSOLUTE);
    }

    /**
     * The year of the date to be checked must be smaller than or equal to the argument {@code max}.<br>
     * If the date to be validated is {@code null}, the constraint evaluates to {@code false}.
     *
     * @param max the lower bound of the absolute range
     * @return a {@code YearRange} constraint object
     */
    public static YearRange maxAbsolute(int max) {
        return new YearRange(false, null, max, YearRange.RangeType.ABSOLUTE);
    }

    /**
     * The year of the date to be checked must be smaller than or equal to the argument {@code max}.<br>
     * If the date to be validated is {@code null}, the constraint evaluates to {@code true}.
     *
     * @param max the lower bound of the absolute range
     * @return a {@code YearRange} constraint object
     */
    public static YearRange maxAbsoluteOrNull(int max) {
        return new YearRange(true, null, max, YearRange.RangeType.ABSOLUTE);
    }

}

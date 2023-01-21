package de.swa.clv.constraints;

public abstract class RegEx extends Constraint {

    /**
     * The element that should be validated must match one of the given regular expressions.
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code String} (string is evaluated)</li>
     * <li>{@code Enum<?>} (name of the enumeration is evaluated)</li>
     * </ul>
     * <p/>
     *
     * @param regex the regular expressions one of which must match the element
     */
    public static RegExAny any(String... regex) {
        assertValuesAndSizeOk(regex);
        return new RegExAny(false, regex);
    }

    public static RegExAny anyOrNull(String... regex) {
        assertValuesAndSizeOk(regex);
        return new RegExAny(true, regex);
    }

    /**
     * The element that should be validated must not match any of the given regular expressions.
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code String} (string is evaluated)</li>
     * <li>{@code Enum<?>} (name of the enumeration is evaluated)</li>
     * </ul>
     * <p/>
     *
     * @param regex the regular expressions one of which must match the element
     */
    public static RegExNone none(String... regex) {
        assertValuesAndSizeOk(regex);
        return new RegExNone(true, regex);
    }

    public static RegExNone noneNorNull(String... regex) {
        assertValuesAndSizeOk(regex);
        return new RegExNone(false, regex);
    }

}

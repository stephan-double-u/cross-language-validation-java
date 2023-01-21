package de.swa.clv.constraints;

class RegExNone extends RegExConstraint implements IsCreateConstraint, IsUpdateConstraint {

    static final String TYPE = "REGEX_NONE";

    RegExNone(boolean nullEqualsTrue, String... regex) {
        super(regex);
        setNullEqualsTrue(nullEqualsTrue);

    }

    @Override
    public String getToken() {
        return TYPE;
    }
}

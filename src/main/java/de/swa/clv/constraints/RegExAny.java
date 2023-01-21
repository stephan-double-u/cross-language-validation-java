package de.swa.clv.constraints;

class RegExAny extends RegExConstraint implements IsCreateConstraint, IsUpdateConstraint {

    static final String TYPE = "REGEX_ANY";

    RegExAny(boolean nullEqualsTrue, String[] regex) {
        super(regex);
        setNullEqualsTrue(nullEqualsTrue);
    }

    @Override
    public String getToken() {
        return TYPE;
    }
}

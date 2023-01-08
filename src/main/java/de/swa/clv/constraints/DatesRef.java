package de.swa.clv.constraints;

abstract class DatesRef extends Dates {

    private boolean ofUpdate = false;
    private boolean ofCurrent = false;

    public boolean isOfUpdate() {
        return ofUpdate;
    }

    void setOfUpdate(boolean ofUpdate) {
        this.ofUpdate = ofUpdate;
    }

    public boolean isOfCurrent() {
        return ofCurrent;
    }

    void setOfCurrent(boolean ofCurrent) {
        this.ofCurrent = ofCurrent;
    }
}

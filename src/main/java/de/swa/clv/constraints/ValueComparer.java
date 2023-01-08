package de.swa.clv.constraints;

public interface ValueComparer {

    String getToken();

    boolean validate(Object thisValue, Object thatValue);

}

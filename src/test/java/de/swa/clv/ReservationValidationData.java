package de.swa.clv;

import de.swa.clv.constraints.Equals;

public class ReservationValidationData extends ValidationRules<ValidationTesting.Reservation> {

        private ReservationValidationData(Class<ValidationTesting.Reservation> typeClass) {
            super(typeClass);
            mandatory("id");
            immutable("id");
            content("id", Equals.any(1, 2, 3));
        }
        private static ReservationValidationData INSTANCE = new ReservationValidationData(ValidationTesting.Reservation.class);

        public static ReservationValidationData instance() {
            return INSTANCE;
        }
        
    }


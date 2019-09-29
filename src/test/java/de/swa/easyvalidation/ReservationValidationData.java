package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.Equals;

public class ReservationValidationData extends ValidationConditions<EasyValidationTesting.Reservation> {

        private ReservationValidationData(Class<EasyValidationTesting.Reservation> typeClass) {
            super(typeClass);
            mandatory("id");
            immutable("id");
            content("id", Equals.any(1, 2, 3));
        }
        private static ReservationValidationData INSTANCE = new ReservationValidationData(EasyValidationTesting.Reservation.class);

        public static ReservationValidationData instance() {
            return INSTANCE;
        }
        
    }


package com.uket.domain.event.enums;

public enum ReservationUserType {
    TICKETING_ALL, TICKETING_STUDENT;

    public static Boolean isTicketingAll(ReservationUserType reservationUserType) {
        return TICKETING_ALL.equals(reservationUserType);
    }
}

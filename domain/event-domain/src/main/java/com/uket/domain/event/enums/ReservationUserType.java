package com.uket.domain.event.enums;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.exception.EventException;
import lombok.Getter;

@Getter
public enum ReservationUserType {
    TICKETING_ALL("일반인 zone"), TICKETING_STUDENT("재학생 zone");

    private final String value;

    ReservationUserType(String value) {
        this.value = value;
    }

    public static Boolean isTicketingAll(ReservationUserType reservationUserType) {
        return TICKETING_ALL.equals(reservationUserType);
    }

    public static ReservationUserType fromString(String reservationUserType) {
        for (ReservationUserType type : ReservationUserType.values()) {
            if (type.getValue().equals(reservationUserType)) {
                return type;
            }
        }
        throw new EventException(ErrorCode.NOT_FOUND_RESERVATION_USER_TYPE);
    }
}

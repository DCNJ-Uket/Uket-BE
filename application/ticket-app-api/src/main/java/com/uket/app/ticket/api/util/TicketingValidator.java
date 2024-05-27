package com.uket.app.ticket.api.util;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.event.enums.ReservationUserType;
import com.uket.domain.ticket.exception.TicketException;
import com.uket.domain.ticket.repository.TicketRepository;
import com.uket.domain.university.entity.University;
import com.uket.domain.user.entity.Users;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class TicketingValidator {

    private final TicketRepository ticketRepository;

    public boolean validateStudentOfUniversity(Users user, University university) {
        String universityNameOfUser = user.getUniversity().getName();

        return universityNameOfUser.equals(university.getName());
    }

    public void validateReservationUserType(Reservation reservation) {

        if (Boolean.TRUE.equals(ReservationUserType.isTicketingAll(reservation.getType()))){
            return;
        }
        throw new TicketException(ErrorCode.INVALID_RESERVATION_USER_TYPE);
    }

    public void validateDuplicateReservationOfSameShow(Users user, Reservation reservation) {

        Shows show = reservation.getShow();

        if (Boolean.TRUE.equals(ticketRepository.existsByUserAndShow(user, show))) {
            throw new TicketException(ErrorCode.DUPLICATE_RESERVATION_OF_SAME_SHOW);
        }
    }
}

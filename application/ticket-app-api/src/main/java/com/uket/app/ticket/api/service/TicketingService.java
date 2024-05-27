package com.uket.app.ticket.api.service;

import com.uket.app.ticket.api.util.TicketingValidator;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.event.service.ReservationService;
import com.uket.domain.ticket.dto.CreateTicketDto;
import com.uket.domain.ticket.dto.TicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.ticket.exception.TicketException;
import com.uket.domain.ticket.service.TicketService;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.service.UniversityService;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.service.UserService;
import com.uket.modules.redis.lock.aop.DistributedLock;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class TicketingService {

    private final UserService userService;
    private final UniversityService universityService;
    private final TicketService ticketService;
    private final ReservationService reservationService;

    private final TicketingValidator ticketingValidator;

    @Transactional
    @DistributedLock(key = "#reservationId")
    public TicketDto ticketing(Long userId, Long universityId, Long reservationId) {

        Users user = userService.findById(userId);
        University university = universityService.findById(universityId);
        Reservation reservation = reservationService.findById(reservationId);

        increaseReservedCount(reservation);

        CreateTicketDto createTicketDto = generateCreateTicketDto(user, university, reservation);
        Ticket ticket = ticketService.save(createTicketDto);
        return TicketDto.from(ticket);
    }

    public void validateTicketing(Long userId, Long universityId, Long reservationId) {

        Users user = userService.findById(userId);
        University university = universityService.findById(universityId);
        Reservation reservation = reservationService.findById(reservationId);

        if (Boolean.FALSE.equals(ticketingValidator.validateStudentOfUniversity(user, university))) {
            ticketingValidator.validateReservationUserType(reservation);
        }
        ticketingValidator.validateDuplicateReservationOfSameShow(user, reservation);
    }

    private void increaseReservedCount(Reservation reservation) {
        Boolean isSuccess = reservation.increaseReservedCount();
        if (Boolean.FALSE.equals(isSuccess)) {
            throw new TicketException(ErrorCode.FAIL_TICKETING_COUNT);
        }
    }

    private CreateTicketDto generateCreateTicketDto(Users user, University university, Reservation reservation) {
        Shows show = reservation.getShow();
        Events event = show.getEvent();
        TicketStatus ticketStatus = checkTicketStatus(user, university);

        return CreateTicketDto.builder()
                .user(user)
                .event(event)
                .show(show)
                .reservation(reservation)
                .status(ticketStatus)
                .build();
    }

    private TicketStatus checkTicketStatus(Users user, University university) {
        if (Boolean.TRUE.equals(university.validateUniversityByName(user.getUniversity().getName()))) {
            return TicketStatus.BEFORE_ENTER;
        }
        return TicketStatus.BEFORE_PAYMENT;
    }
}

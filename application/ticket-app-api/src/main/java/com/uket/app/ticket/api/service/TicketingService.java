package com.uket.app.ticket.api.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
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

    @DistributedLock(key = "#reservation.getId()")
    public TicketDto ticketing(Long userId, Long universityId, Reservation reservation) {

        Users user = userService.findById(userId);
        University university = universityService.findById(universityId);

        Boolean isSuccess = reservation.increaseReservedCount();
        if (Boolean.FALSE.equals(isSuccess)) {
            throw new TicketException(ErrorCode.FAIL_TICKETING_COUNT);
        }

        CreateTicketDto createTicketDto = generateCreateTicketDto(user, university, reservation);

        Ticket ticket = ticketService.save(createTicketDto);
        return TicketDto.from(ticket);
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
        if (university.validateUniversityByName(user.getUniversity().getName())) {
            return TicketStatus.BEFORE_ENTER;
        }
        return TicketStatus.BEFORE_PAYMENT;
    }
}

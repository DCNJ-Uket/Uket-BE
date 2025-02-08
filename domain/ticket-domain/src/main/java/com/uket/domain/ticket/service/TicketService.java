package com.uket.domain.ticket.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.service.ReservationService;
import com.uket.domain.form.entity.Answer;
import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.Survey;
import com.uket.domain.form.repository.AnswerRepository;
import com.uket.domain.ticket.dto.CancelTicketDto;
import com.uket.domain.ticket.dto.CreateTicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.ticket.exception.TicketException;
import com.uket.domain.ticket.repository.TicketRepository;
import com.uket.domain.user.entity.Users;
import com.uket.modules.redis.lock.aop.DistributedLock;
import java.util.List;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import org.hibernate.Hibernate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class TicketService {

    private final TicketRepository ticketRepository;
    private final AnswerRepository answerRepository;
    private final ReservationService reservationService;

    @DistributedLock(key = "#reservationId")
    public void decreaseReservedCount(Long reservationId) {
        Reservation reservation = reservationService.findById(reservationId);
        Boolean isSuccess = reservation.decreaseReservedCount();

        if (Boolean.FALSE.equals(isSuccess)) {
            throw new TicketException(ErrorCode.FAIL_TICKET_CANCEL);
        }
    }

    @Transactional
    public Ticket save(CreateTicketDto createTicketDto) {

        Users user = createTicketDto.user();
        Reservation reservation = createTicketDto.reservation();

        if(Boolean.TRUE.equals(ticketRepository.existsByUserAndReservationAndStatusNot(user, reservation, TicketStatus.RESERVATION_CANCEL))){
            throw new TicketException(ErrorCode.ALREADY_EXIST_TICKET);
        }

        Ticket ticket = Ticket.builder()
            .user(user)
            .reservation(reservation)
            .event(createTicketDto.event())
            .show(createTicketDto.show())
            .status(createTicketDto.status())
            .ticketNo(UUID.randomUUID().toString())
            .build();

        return ticketRepository.save(ticket);
    }

    @Transactional(readOnly = true)
    public Ticket findById(Long ticketId) {
        return ticketRepository.findById(ticketId)
            .orElseThrow(() -> new TicketException(ErrorCode.NOT_FOUND_TICKET));
    }

    public void checkTicketOwner(Long userId, Long ticketId) {
        if (Boolean.FALSE.equals(ticketRepository.existsByUserIdAndId(userId, ticketId))) {
            throw new TicketException(ErrorCode.INVALID_ACCESS_TICKET);
        }
    }

    @Transactional(readOnly = true)
    public List<Ticket> findAllTicketsByUserId(Long userId) {
        List<TicketStatus> excludedStatuses = List.of(TicketStatus.RESERVATION_CANCEL, TicketStatus.EXPIRED);
        return ticketRepository.findValidTicketsByUserId(userId, excludedStatuses);
    }

    @Transactional
    public void deleteAllUserTickets(Long userId) {
        List<Ticket> tickets = ticketRepository.findAllByUserIdAndStatusNotWithReservation(userId, TicketStatus.RESERVATION_CANCEL);
        for(Ticket ticket : tickets) {
            this.decreaseReservedCount(ticket.getReservation().getId());
        }
        ticketRepository.deleteAllByUserId(userId);
    }

    @Transactional
    public void deleteAllTicketAnswers(Long userId, Long ticketId) {
        Ticket ticket = ticketRepository.findTicketWithEventAndSurvey(ticketId).orElseThrow(() ->
            new TicketException(ErrorCode.NOT_FOUND_TICKET));

        Survey survey = ticket.getEvent().getSurvey();
        Hibernate.initialize(survey.getForms());
        List<Form> forms = survey.getForms();

        answerRepository.deleteAnswersByUserIdAndForms(userId, forms);
    }

    @Transactional
    public CancelTicketDto cancelTicketByUserIdAndId(Long userId, Long ticketId) {
        Ticket ticket = ticketRepository.findByUserIdAndId(userId, ticketId)
            .orElseThrow(() -> new TicketException(ErrorCode.FAIL_TO_FIND_TICKET));

        ticket.cancel();
        ticket.updateDeletedAt();
        ticketRepository.save(ticket);

        return new CancelTicketDto(ticket.getId(), ticket.getStatus().getValue(), ticket.getReservation().getId());
    }

    public void validateTicketStatus(Long ticketId) {
        Ticket ticket = ticketRepository.findById(ticketId)
            .orElseThrow(() -> new TicketException(ErrorCode.FAIL_TO_FIND_TICKET));

        TicketStatus ticketStatus = ticket.getStatus();

        if(ticketStatus == TicketStatus.FINISH_ENTER) {
            throw new TicketException(ErrorCode.ALREADY_ENTER_TICKET);
        } else if (ticketStatus == TicketStatus.EXPIRED) {
            throw new TicketException(ErrorCode.EXPIRED_TICKET);
        }
    }

    public Ticket updateTicketStatus(Long ticketId, TicketStatus ticketStatus) {
        Ticket ticket = ticketRepository.findById(ticketId)
            .orElseThrow(() -> new TicketException(ErrorCode.FAIL_TO_FIND_TICKET));

        Ticket updatedTicket = ticket.updateStatus(ticketStatus);
        return ticketRepository.save(updatedTicket);
    }

}

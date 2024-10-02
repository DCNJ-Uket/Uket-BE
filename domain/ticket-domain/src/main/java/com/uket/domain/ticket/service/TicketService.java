package com.uket.domain.ticket.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.service.ReservationService;
import com.uket.domain.ticket.dto.CancelTicketDto;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.ticket.dto.CreateTicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.ticket.exception.TicketException;
import com.uket.domain.ticket.repository.TicketRepository;
import com.uket.domain.user.entity.Users;
import com.uket.modules.redis.lock.aop.DistributedLock;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class TicketService {

    private final TicketRepository ticketRepository;
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

        /*
        if(Boolean.TRUE.equals(ticketRepository.existsByUserAndReservationAndStatusNot(user, reservation, TicketStatus.RESERVATION_CANCEL))){
            throw new TicketException(ErrorCode.ALREADY_EXIST_TICKET);
        }
         */

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
        return ticketRepository.findAllByUserIdAndStatusNot(userId, TicketStatus.RESERVATION_CANCEL);
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


    @Transactional
    public Ticket updateTicketStatus(Long ticketId, TicketStatus ticketStatus) {
        Ticket ticket = ticketRepository.findById(ticketId)
                .orElseThrow(() -> new TicketException(ErrorCode.FAIL_TO_FIND_TICKET));

        Ticket updatedTicket = ticket.updateStatus(ticketStatus);
        return ticketRepository.save(updatedTicket);
    }


    @Transactional(readOnly = true)
    public Page<CheckTicketDto> searchAllTickets(int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        Page<Ticket> tickets =  ticketRepository.findAll(pageable);
        return tickets.map(CheckTicketDto::from);
    }

    @Transactional(readOnly = true)
    public Page<CheckTicketDto> searchTicketsByStatus(TicketStatus status, int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        Page<Ticket> tickets = ticketRepository.findByStatus(status, pageable);
        return tickets.map(CheckTicketDto::from);
    }

    @Transactional(readOnly = true)
    public Page<CheckTicketDto> searchTicketsByUserName(String userName, int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        Page<Ticket> tickets = ticketRepository.findByUserName(userName, pageable);
        return tickets.map(CheckTicketDto::from);
    }

    @Transactional(readOnly = true)
    public Page<CheckTicketDto> searchTicketsByPhoneNumber(String phoneNumber, int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        Page<Ticket> tickets = ticketRepository.findByUserUserDetailsPhoneNumber(phoneNumber, pageable);
        return tickets.map(CheckTicketDto::from);
    }

    @Transactional(readOnly = true)
    public Page<CheckTicketDto> searchTicketsByShowStartDate(LocalDateTime startDate, int page, int size) {
        Pageable pageable = PageRequest.of(page, size);
        Page<Ticket> tickets = ticketRepository.findByShowStartDate(startDate, pageable);
        return tickets.map(CheckTicketDto::from);
    }

}

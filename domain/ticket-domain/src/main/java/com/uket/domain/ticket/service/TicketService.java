package com.uket.domain.ticket.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.ticket.dto.CreateTicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.exception.TicketException;
import com.uket.domain.ticket.repository.TicketRepository;
import com.uket.domain.user.entity.Users;
import java.util.List;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class TicketService {

    private final TicketRepository ticketRepository;

    public Ticket save(CreateTicketDto createTicketDto) {

        Users user = createTicketDto.user();
        Reservation reservation = createTicketDto.reservation();

        if(Boolean.TRUE.equals(ticketRepository.existsByUserAndReservation(user, reservation))){
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

    public Ticket findById(Long ticketId) {
        return ticketRepository.findById(ticketId)
                .orElseThrow(() -> new TicketException(ErrorCode.NOT_FOUND_TICKET));
    }

    public void checkTicketOwner(Long userId, Long ticketId) {
        if (Boolean.FALSE.equals(ticketRepository.existsByUserIdAndId(userId, ticketId))) {
            throw new TicketException(ErrorCode.INVALID_ACCESS_TICKET);
        }
    }

    public List<Ticket> findAllTicketsByUserId(Long userId) {
        return ticketRepository.findAllByUserId(userId);
    }

}

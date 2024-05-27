package com.uket.domain.ticket.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.ticket.dto.CreateTicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.exception.TicketException;
import com.uket.domain.ticket.repository.TicketRepository;
import com.uket.domain.user.entity.Users;
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
                .build();

        return ticketRepository.save(ticket);
    }
}

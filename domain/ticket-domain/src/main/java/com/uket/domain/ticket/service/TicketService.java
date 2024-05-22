package com.uket.domain.ticket.service;

import com.uket.domain.ticket.dto.CreateTicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.repository.TicketRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class TicketService {

    private final TicketRepository ticketRepository;

    public Ticket save(CreateTicketDto createTicketDto) {

        return null;
    }
}

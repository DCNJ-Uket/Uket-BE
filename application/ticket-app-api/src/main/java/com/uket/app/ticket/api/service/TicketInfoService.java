package com.uket.app.ticket.api.service;

import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.service.TicketService;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class TicketInfoService {
    private final TicketService ticketService;
    public List<CheckTicketDto> getUserTickets(Long userId) {
        List<Ticket> tickets = ticketService.findAllTicketsByUserId(userId);
        return tickets.stream().map(CheckTicketDto::from).toList();
    }
}

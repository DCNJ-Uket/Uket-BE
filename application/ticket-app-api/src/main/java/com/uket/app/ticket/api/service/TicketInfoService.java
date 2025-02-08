package com.uket.app.ticket.api.service;

import com.uket.domain.event.entity.Events;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.service.TicketService;
import com.uket.modules.aws.s3.service.S3Service;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class TicketInfoService {
    private final TicketService ticketService;
    private final S3Service s3Service;

    public List<CheckTicketDto> getUserTickets(Long userId) {
        List<Ticket> tickets = ticketService.findAllTicketsByUserId(userId);
        return tickets.stream().map(ticket ->
            {
                Events event = ticket.getEvent();
                return CheckTicketDto.of(ticket, s3Service.getEventMainImage(event.getMainImagePath()));
            }
        ).toList();
    }
}

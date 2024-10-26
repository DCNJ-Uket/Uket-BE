package com.uket.app.admin.api.controller.impl;

import com.uket.app.admin.api.controller.TicketApi;
import com.uket.app.admin.api.dto.response.EnterShowResponse;
import com.uket.app.admin.api.dto.response.UpdateTicketStatusResponse;
import com.uket.app.admin.api.service.EnterShowService;
import com.uket.domain.ticket.dto.TicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.ticket.service.TicketService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class TicketController implements TicketApi {

    private final EnterShowService enterShowService;
    private final TicketService ticketService;

    @Override
    public ResponseEntity<EnterShowResponse> enterShow(String ticketToken) {

        TicketDto ticketDto = enterShowService.enterShow(ticketToken);

        EnterShowResponse response = EnterShowResponse.of(ticketDto);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<UpdateTicketStatusResponse> updateTicketStatus(Long ticketId, TicketStatus ticketStatus) {

        Ticket ticket = ticketService.updateTicketStatus(ticketId, ticketStatus);

        UpdateTicketStatusResponse response = UpdateTicketStatusResponse.from(ticket);
        return ResponseEntity.ok(response);
    }
}

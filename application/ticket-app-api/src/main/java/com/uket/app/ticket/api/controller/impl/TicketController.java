package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.TicketApi;
import com.uket.app.ticket.api.dto.request.TicketingRequest;
import com.uket.app.ticket.api.dto.response.TicketingResponse;
import com.uket.app.ticket.api.service.TicketingService;
import com.uket.domain.ticket.dto.TicketDto;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class TicketController implements TicketApi {

    private final TicketingService ticketingService;

    @Override
    public ResponseEntity<TicketingResponse> ticketing(Long userId, TicketingRequest request) {

        ticketingService.validateTicketing(userId, request.universityId(), request.reservationId());
        ticketingService.increaseReservedCount(request.reservationId());

        TicketDto ticket = ticketingService.ticketing(userId, request.universityId(), request.reservationId());

        TicketingResponse response = TicketingResponse.of(true, ticket);
        return ResponseEntity.ok(response);
    }
}

package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.TicketApi;
import com.uket.app.ticket.api.dto.request.TicketingRequest;
import com.uket.app.ticket.api.dto.response.TicketingResponse;
import com.uket.app.ticket.api.service.QRCodeService;
import com.uket.app.ticket.api.service.TicketingService;
import com.uket.domain.ticket.dto.TicketDto;
import com.uket.domain.ticket.service.TicketService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class TicketController implements TicketApi {

    private final TicketService ticketService;
    private final TicketingService ticketingService;
    private final QRCodeService qrCodeService;

    @Override
    public ResponseEntity<TicketingResponse> ticketing(Long userId, TicketingRequest request) {

        ticketingService.validateTicketing(userId, request.universityId(), request.reservationId());
        ticketingService.increaseReservedCount(request.reservationId());

        TicketDto ticket = ticketingService.ticketing(userId, request.universityId(), request.reservationId());

        TicketingResponse response = TicketingResponse.of(true, ticket);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<byte[]> getQRCode(Long userId, Long ticketId) {

        ticketService.checkTicketOwner(userId, ticketId);

        byte[] qrCode = qrCodeService.generateTicketQRCode(ticketId);

        return ResponseEntity.ok()
                .contentType(MediaType.IMAGE_PNG)
                .body(qrCode);
    }

    @Override
    public ResponseEntity<Void> cancelTicket(Long userId, Long ticketId) {
        ticketService.cancelTicketByUserIdAndId(userId, ticketId);
        return ResponseEntity.noContent().build();
    }
}

package com.uket.app.admin.api.controller.impl;

import com.uket.app.admin.api.controller.TicketApi;
import com.uket.app.admin.api.dto.response.CustomPageResponse;
import com.uket.app.admin.api.dto.response.EnterShowResponse;
import com.uket.app.admin.api.dto.response.TicketResponse;
import com.uket.app.admin.api.dto.response.UpdateTicketStatusResponse;
import com.uket.app.admin.api.service.EnterShowService;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.ticket.dto.TicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.ticket.service.TicketService;
import java.time.LocalDateTime;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
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

    @Override
    public ResponseEntity<CustomPageResponse<TicketResponse>> getAllTickets(int page, int size) {
        Page<CheckTicketDto> tickets = ticketService.getAllTickets(page-1, size);
        Page<TicketResponse> ticketResponses = tickets.map(TicketResponse::from);
        CustomPageResponse<TicketResponse> customResponse = new CustomPageResponse<>(ticketResponses);
        return ResponseEntity.ok(customResponse);
    }

    /*
    @Override
    public ResponseEntity<Page<TicketResponse>> getTicketsByUserName(String userName, int page, int size) {
        Page<TicketResponse> ticketResponses = ticketService.getTicketsByUserName(userName, page, size);
        return ResponseEntity.ok(ticketResponses);
    }

    @Override
    public ResponseEntity<Page<TicketResponse>> getTicketsByStatus(TicketStatus status, int page, int size) {
        Page<TicketResponse> ticketResponses = ticketService.getTicketsByStatus(status, page, size);
        return ResponseEntity.ok(ticketResponses);
    }

    @Override
    public ResponseEntity<Page<TicketResponse>> getTicketsByPhoneNumber(String phoneNumber, int page, int size) {
        Page<TicketResponse> ticketResponses = ticketService.getTicketsByPhoneNumber(phoneNumber, page, size);
        return ResponseEntity.ok(ticketResponses);
    }

    @Override
    public ResponseEntity<Page<TicketResponse>> getTicketsByShowDate(LocalDateTime showDate, int page, int size) {
        Page<TicketResponse> ticketResponses = ticketService.getTicketsByShowStartDate(showDate, page, size);
        return ResponseEntity.ok(ticketResponses);
    }
    */
}

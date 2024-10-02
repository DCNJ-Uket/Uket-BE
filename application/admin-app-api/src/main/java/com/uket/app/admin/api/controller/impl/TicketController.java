package com.uket.app.admin.api.controller.impl;

import com.uket.app.admin.api.controller.TicketApi;
import com.uket.app.admin.api.dto.request.PhoneNumberRequest;
import com.uket.app.admin.api.dto.request.UserNameRequest;
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
    public ResponseEntity<CustomPageResponse<TicketResponse>> searchAllTickets(int page, int size) {
        Page<CheckTicketDto> tickets = ticketService.searchAllTickets(page-1, size);
        Page<TicketResponse> ticketResponses = tickets.map(TicketResponse::from);
        CustomPageResponse<TicketResponse> customResponse = new CustomPageResponse<>(ticketResponses);
        return ResponseEntity.ok(customResponse);
    }

    @Override
    public ResponseEntity<CustomPageResponse<TicketResponse>> searchTicketsByStatus(TicketStatus status, int page, int size) {
        Page<CheckTicketDto> tickets = ticketService.searchTicketsByStatus(status,page-1, size);
        Page<TicketResponse> ticketResponses = tickets.map(TicketResponse::from);
        CustomPageResponse<TicketResponse> customResponse = new CustomPageResponse<>(ticketResponses);
        return ResponseEntity.ok(customResponse);
    }


    @Override
    public ResponseEntity<CustomPageResponse<TicketResponse>> searchTicketsByUserName(UserNameRequest userNameRequest, int page, int size) {
        Page<CheckTicketDto> tickets = ticketService.searchTicketsByUserName(userNameRequest.userName(), page-1, size);
        Page<TicketResponse> ticketResponses = tickets.map(TicketResponse::from);
        CustomPageResponse<TicketResponse> customResponse = new CustomPageResponse<>(ticketResponses);
        return ResponseEntity.ok(customResponse);
    }

    @Override
    public ResponseEntity<CustomPageResponse<TicketResponse>> searchTicketsByPhoneNumber(PhoneNumberRequest phoneNumberRequest, int page, int size) {
        Page<CheckTicketDto> tickets = ticketService.searchTicketsByPhoneNumber(phoneNumberRequest.phoneNumber(), page-1, size);
        Page<TicketResponse> ticketResponses = tickets.map(TicketResponse::from);
        CustomPageResponse<TicketResponse> customResponse = new CustomPageResponse<>(ticketResponses);
        return ResponseEntity.ok(customResponse);
    }
    /*
    @Override
    public ResponseEntity<Page<TicketResponse>> searchTicketsByShowDate(LocalDateTime showDate, int page, int size) {
        Page<TicketResponse> ticketResponses = ticketService.searchTicketsByShowStartDate(showDate, page-1, size);
        return ResponseEntity.ok(ticketResponses);
    }
    */
}

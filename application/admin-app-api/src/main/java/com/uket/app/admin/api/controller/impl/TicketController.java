package com.uket.app.admin.api.controller.impl;

import com.uket.app.admin.api.controller.TicketApi;
import com.uket.app.admin.api.dto.request.CreatedAtRequest;
import com.uket.app.admin.api.dto.request.ModifiedAtRequest;
import com.uket.app.admin.api.dto.request.PhoneNumberRequest;
import com.uket.app.admin.api.dto.request.SearchRequest;
import com.uket.app.admin.api.dto.request.ShowDateRequest;
import com.uket.app.admin.api.dto.request.UserNameRequest;
import com.uket.app.admin.api.dto.response.CustomPageResponse;
import com.uket.app.admin.api.dto.response.EnterShowResponse;
import com.uket.app.admin.api.dto.response.TicketResponse;
import com.uket.app.admin.api.dto.response.UpdateTicketStatusResponse;
import com.uket.app.admin.api.enums.TicketSearchType;
import com.uket.app.admin.api.exception.AdminException;
import com.uket.app.admin.api.service.EnterShowService;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.enums.ReservationUserType;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.ticket.dto.TicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.ticket.service.TicketService;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
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
        Pageable pageable = PageRequest.of(page - 1, size);
        Page<CheckTicketDto> tickets = ticketService.searchAllTickets(pageable);
        Page<TicketResponse> ticketResponses = tickets.map(TicketResponse::from);
        CustomPageResponse<TicketResponse> customResponse = new CustomPageResponse<>(ticketResponses);
        return ResponseEntity.ok(customResponse);
    }

    @Override
    public ResponseEntity<CustomPageResponse<TicketResponse>> searchTickets(
        TicketSearchType searchType,
        SearchRequest searchRequest,
        int page,
        int size
    ) {
        Pageable pageable = PageRequest.of(page - 1, size);
        Page<CheckTicketDto> tickets;

        switch (searchType) {
            case STATUS:
                tickets = ticketService.searchTicketsByStatus(searchRequest.status(), pageable);
                break;
            case USER_NAME:
                tickets = ticketService.searchTicketsByUserName(searchRequest.userName(), pageable);
                break;
            case PHONE_NUMBER:
                tickets = ticketService.searchTicketsByPhoneNumber(searchRequest.phoneNumber(), pageable);
                break;
            case SHOW_DATE:
                tickets = ticketService.searchTicketsByShowStartDate(searchRequest.showDate(), pageable);
                break;
            case RESERVATION_USER_TYPE:
                tickets = ticketService.searchTicketsByReservationUserType(searchRequest.reservationUserType(), pageable);
                break;
            case CREATED_AT:
                tickets = ticketService.searchTicketsByCreatedAt(searchRequest.createdAt(), pageable);
                break;
            case MODIFIED_AT:
                tickets = ticketService.searchTicketsByModifiedAt(searchRequest.modifiedAt(),pageable);
                break;
            default:
                throw new AdminException(ErrorCode.INVALID_SEARCH_TYPE);
        }

        Page<TicketResponse> ticketResponses = tickets.map(TicketResponse::from);
        CustomPageResponse<TicketResponse> customResponse = new CustomPageResponse<>(ticketResponses);
        return ResponseEntity.ok(customResponse);
    }
}

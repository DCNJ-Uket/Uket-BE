package com.uket.app.admin.api.controller.impl;

import com.uket.app.admin.api.aop.LimitRequest;
import com.uket.app.admin.api.controller.TicketApi;
import com.uket.app.admin.api.dto.request.SearchRequest;
import com.uket.app.admin.api.dto.response.CustomPageResponse;
import com.uket.app.admin.api.dto.response.EnterShowResponse;
import com.uket.app.admin.api.dto.response.LiveEnterUserResponse;
import com.uket.app.admin.api.dto.response.TicketResponse;
import com.uket.app.admin.api.dto.response.UpdateTicketStatusResponse;
import com.uket.app.admin.api.enums.TicketSearchType;
import com.uket.app.admin.api.exception.AdminException;
import com.uket.app.admin.api.service.EnterShowService;
import com.uket.app.admin.api.service.search.TicketSearcher;
import com.uket.app.admin.api.service.LiveEnterUserDto;
import com.uket.app.admin.api.service.TicketAdminService;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.ticket.dto.TicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.ticket.service.TicketService;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class TicketController implements TicketApi {

    private final EnterShowService enterShowService;
    private final TicketService ticketService;
    private final List<TicketSearcher> ticketSearchers;
    private final TicketAdminService ticketAdminService;

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
        Page<TicketResponse> ticketResponses = ticketService.searchAllTickets(PageRequest.of(page - 1, size))
                .map(TicketResponse::from);

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
        Page<TicketResponse> ticketResponses = ticketSearchers.stream()
                .filter(ticketSearcher -> ticketSearcher.isSupport(searchType))
                .findFirst().orElseThrow(() -> new AdminException(ErrorCode.INVALID_SEARCH_TYPE))
                .search(searchRequest, PageRequest.of(page - 1, size))
                .map(TicketResponse::from);

        CustomPageResponse<TicketResponse> customResponse = new CustomPageResponse<>(ticketResponses);
        return ResponseEntity.ok(customResponse);
    }

    @Override
    @LimitRequest
    public ResponseEntity<CustomPageResponse<LiveEnterUserResponse>> searchLiveEnterUsers(int page, int size) {
        Page<LiveEnterUserDto> liveEnterUserDtos = ticketAdminService.searchLiveEnterUsers(PageRequest.of(page - 1, size));

        CustomPageResponse<LiveEnterUserResponse> customResponse = new CustomPageResponse<>(liveEnterUserDtos.map(LiveEnterUserResponse::from));
        return ResponseEntity.ok(customResponse);
    }
}

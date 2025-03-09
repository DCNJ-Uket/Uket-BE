package com.uket.app.admin.api.controller.impl;

import com.uket.app.admin.api.aop.LimitRequest;
import com.uket.app.admin.api.controller.TicketApi;
import com.uket.app.admin.api.dto.CheckTicketingDto;
import com.uket.app.admin.api.dto.request.SearchRequest;
import com.uket.app.admin.api.dto.response.CustomPageResponse;
import com.uket.app.admin.api.dto.response.EnterShowResponse;
import com.uket.app.admin.api.dto.response.LiveEnterUserResponse;
import com.uket.app.admin.api.dto.response.TicketResponse;
import com.uket.app.admin.api.dto.response.TicketingResponse;
import com.uket.app.admin.api.dto.response.UpdateTicketStatusResponse;
import com.uket.app.admin.api.enums.TicketSearchType;
import com.uket.app.admin.api.exception.AdminException;
import com.uket.app.admin.api.aop.ApplyMasking;
import com.uket.app.admin.api.service.EnterShowService;
import com.uket.app.admin.api.service.TicketSearchService;
import com.uket.app.admin.api.service.search.TicketSearcher;
import com.uket.app.admin.api.dto.LiveEnterUserDto;
import com.uket.app.admin.api.service.TicketAdminService;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.service.EventService;
import com.uket.domain.form.service.FormService;
import com.uket.domain.ticket.dto.AdminCheckTicketDto;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.ticket.dto.TicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.ticket.repository.TicketRepository;
import com.uket.domain.ticket.service.TicketService;
import com.uket.domain.user.service.UserService;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
@RequiredArgsConstructor
public class TicketController implements TicketApi {

    private final EnterShowService enterShowService;
    private final TicketService ticketService;
    private final List<TicketSearcher> ticketSearchers;
    private final TicketAdminService ticketAdminService;
    private final EventService eventService;
    private final FormService formService;
    private final UserService userService;
    private final TicketSearchService ticketSearchService;
    private final TicketRepository ticketRepository;

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
    @ApplyMasking(typeValue = TicketingResponse.class)
    public ResponseEntity<CustomPageResponse<TicketingResponse>> searchTickets(
            TicketSearchType searchType,
            SearchRequest searchRequest,
            int page,
            int size
    ) {
        PageRequest pageRequest = PageRequest.of(page - 1, size, Sort.by(Direction.DESC, "createdAt"));
        Page<AdminCheckTicketDto> ticketsPage;

        if(searchType == TicketSearchType.NONE) {
            ticketsPage = ticketService.searchAllTickets(pageRequest);
        } else {
            ticketsPage = ticketSearchers.stream()
                .filter(ticketSearcher -> ticketSearcher.isSupport(searchType))
                .findFirst().orElseThrow(() -> new AdminException(ErrorCode.INVALID_SEARCH_TYPE))
                .search(searchRequest, pageRequest);
        }

        List<AdminCheckTicketDto> tickets = ticketsPage.getContent();
        List<CheckTicketingDto> ticketingDtos = ticketSearchService.searchAllUserAnswersFromTickets(tickets);

        CustomPageResponse<TicketingResponse> customResponse =
                new CustomPageResponse<>(
                        new PageImpl<>(
                                ticketingDtos.stream().map(TicketingResponse::from).toList(),
                                pageRequest,
                                ticketsPage.getTotalElements()
                        )
                );

        return ResponseEntity.ok(customResponse);
    }

    @Override
    @LimitRequest
    @ApplyMasking(typeValue = LiveEnterUserResponse.class)
    public ResponseEntity<CustomPageResponse<LiveEnterUserResponse>> searchLiveEnterUsers(int page, int size) {
        Page<LiveEnterUserDto> liveEnterUserDtos = ticketAdminService.searchLiveEnterUsers(
                PageRequest.of(page - 1, size));

        CustomPageResponse<LiveEnterUserResponse> customResponse = new CustomPageResponse<>(
                liveEnterUserDtos.map(LiveEnterUserResponse::from));
        return ResponseEntity.ok(customResponse);
    }
}

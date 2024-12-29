package com.uket.app.admin.api.controller.impl;

import com.uket.app.admin.api.aop.LimitRequest;
import com.uket.app.admin.api.controller.TicketApi;
import com.uket.app.admin.api.dto.CheckTicketingDto;
import com.uket.app.admin.api.dto.request.SearchRequest;
import com.uket.app.admin.api.dto.response.CustomPageResponse;
import com.uket.app.admin.api.dto.response.EnterShowResponse;
import com.uket.app.admin.api.dto.response.LiveEnterUserResponse;
import com.uket.app.admin.api.dto.response.TicketResponse;
import com.uket.app.admin.api.dto.response.UpdateTicketStatusResponse;
import com.uket.app.admin.api.enums.TicketSearchType;
import com.uket.app.admin.api.exception.AdminException;
import com.uket.app.admin.api.aop.ApplyMasking;
import com.uket.app.admin.api.service.EnterShowService;
import com.uket.app.admin.api.service.search.TicketSearcher;
import com.uket.app.admin.api.service.LiveEnterUserDto;
import com.uket.app.admin.api.service.TicketAdminService;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.service.EventService;
import com.uket.domain.form.dto.AnswerDto;
import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.Survey;
import com.uket.domain.form.service.FormService;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.ticket.dto.TicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.ticket.service.TicketService;
import com.uket.domain.user.service.UserService;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
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
    private final EventService eventService;
    private final FormService formService;
    private final UserService userService;

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
    @ApplyMasking(typeValue = TicketResponse.class)
    public ResponseEntity<CustomPageResponse<CheckTicketingDto>> searchAllTickets(int page, int size) {
        // 1. JWT가 유효한지 확인, 어드민 계정인지 확인 -> 생략
        // 2. 해당 어드민 계정이 관리하는 event get -> 생략 & 대체
        // 3. ticket list get -> tickets
        // 4. ticket 소유자마다, 해당 event에 대한 answer list get ->
        // 5. 3, 4번의 내용을 합치기

        Page<CheckTicketDto> ticketsPage = ticketService.searchAllTickets(PageRequest.of(page - 1, size));
        List<CheckTicketDto> tickets = ticketsPage.getContent();

        Long eventId = tickets.getFirst().eventId();
        Survey survey = eventService.findSurveyById(eventId);
        List<Form> forms = formService.findFormsBySurveyId(survey.getId());

        List<CheckTicketingDto> ticketingDtos = tickets.stream()
                .map(ticket -> {
                    Long userId = ticket.userId();
                    List<AnswerDto> answers = forms.stream()
                            .map(form -> formService.findAnswerByFormIdAndUserId(form.getId(), userId))
                            .toList();
                    return CheckTicketingDto.of(ticket, answers);
                }).toList();

        CustomPageResponse<CheckTicketingDto> customResponse = new CustomPageResponse<>(new PageImpl<CheckTicketingDto>(ticketingDtos, PageRequest.of(page - 1, size),
                ticketsPage.getTotalElements()));
        return ResponseEntity.ok(customResponse);
    }

    @Override
    @ApplyMasking(typeValue = TicketResponse.class)
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
    @ApplyMasking(typeValue = LiveEnterUserResponse.class)
    public ResponseEntity<CustomPageResponse<LiveEnterUserResponse>> searchLiveEnterUsers(int page, int size) {
        Page<LiveEnterUserDto> liveEnterUserDtos = ticketAdminService.searchLiveEnterUsers(PageRequest.of(page - 1, size));

        CustomPageResponse<LiveEnterUserResponse> customResponse = new CustomPageResponse<>(liveEnterUserDtos.map(LiveEnterUserResponse::from));
        return ResponseEntity.ok(customResponse);
    }
}

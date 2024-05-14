package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.EventApi;
import com.uket.app.ticket.api.dto.response.ShowResponse;
import com.uket.app.ticket.api.dto.response.TicketingResponse;
import com.uket.domain.event.dto.ShowDto;
import com.uket.domain.event.dto.TicketingDto;
import com.uket.domain.event.service.EventService;
import com.uket.domain.event.service.ShowService;
import com.uket.domain.event.service.TicketingService;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class EventController implements EventApi {

    private final EventService eventService;
    private final ShowService showService;
    private final TicketingService ticketingService;

    @Override
    public ResponseEntity<ShowResponse> getShows(Long eventId) {

        String universityName = eventService.findUniversityNameByEventId(eventId);
        List<ShowDto> shows = showService.findByEventId(eventId);

        ShowResponse response = ShowResponse.of(universityName, shows);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<TicketingResponse> getPossibleTicketings(Long showId) {
        String showName = showService.findNameById(showId);
        List<TicketingDto> ticketings = ticketingService.findByShowId(showId);

        TicketingResponse response = TicketingResponse.of(showName, ticketings);
        return ResponseEntity.ok(response);
    }
}

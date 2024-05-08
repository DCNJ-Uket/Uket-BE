package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.EventApi;
import com.uket.app.ticket.api.dto.response.ShowResponse;
import com.uket.app.ticket.api.dto.response.TicketingResponse;
import com.uket.domain.event.dto.ShowDto;
import com.uket.domain.event.dto.TicketingDto;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Shows;
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

        Events event = eventService.findById(eventId);
        String university = eventService.getUniversityName(eventId);
        List<ShowDto> shows = showService.findByEvent(event);

        ShowResponse response = ShowResponse.of(university, shows);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<TicketingResponse> getPossibleTicketings(Long showId) {
        Shows show = showService.findById(showId);
        List<TicketingDto> ticketings = ticketingService.findByShow(show);

        TicketingResponse response = TicketingResponse.of(show.getName(), ticketings);
        return ResponseEntity.ok(response);
    }
}

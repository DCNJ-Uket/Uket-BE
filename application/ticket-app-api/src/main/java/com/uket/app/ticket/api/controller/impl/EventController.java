package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.EventApi;
import com.uket.app.ticket.api.dto.response.ShowResponse;
import com.uket.domain.event.dto.ShowDto;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.service.EventService;
import com.uket.domain.event.service.ShowService;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class EventController implements EventApi {

    private final EventService eventService;
    private final ShowService showService;

    @Override
    public ResponseEntity<ShowResponse> getShows(Long eventId) {

        Events event = eventService.findById(eventId);
        String university = eventService.getUniversityName(eventId);
        List<ShowDto> shows = showService.findByEvent(event);

        ShowResponse response = ShowResponse.of(university, shows);
        return ResponseEntity.ok(response);
    }
}

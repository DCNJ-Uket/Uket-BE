package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.EventApi;
import com.uket.app.ticket.api.dto.response.CurrentEventResponse;
import com.uket.app.ticket.api.service.UniversityEventService;
import com.uket.domain.event.entity.Events;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class EventController implements EventApi {

    private final UniversityEventService universityEventService;

    @Override
    public ResponseEntity<CurrentEventResponse> getCurrentEventOfUniversity(String university) {

        Events event = universityEventService.getCurrentEventOfUniversity(university);

        CurrentEventResponse response = CurrentEventResponse.from(event);
        return ResponseEntity.ok(response);
    }
}

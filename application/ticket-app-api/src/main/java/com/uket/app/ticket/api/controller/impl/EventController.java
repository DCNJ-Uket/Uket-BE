package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.EventApi;
import com.uket.app.ticket.api.dto.response.CurrentEventResponse;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.exception.EventException;
import com.uket.domain.event.service.EventService;
import com.uket.domain.university.service.UniversityService;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class EventController implements EventApi {

    private final UniversityService universityService;
    private final EventService eventService;

    @Override
    public ResponseEntity<CurrentEventResponse> getCurrentEventOfUniversity(Long userId,
            String university) {

        Optional<Long> currentEvent = universityService.getCurrentEvent(university);
        if (currentEvent.isPresent()) {
            Events event = eventService.findById(currentEvent.get());
            CurrentEventResponse response = CurrentEventResponse.from(event);
            return ResponseEntity.ok(response);
        }
        throw new EventException(ErrorCode.NOT_FOUND_EVENT);
    }
}

package com.uket.app.ticket.api.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.exception.EventException;
import com.uket.domain.event.service.EventService;
import com.uket.domain.university.service.UniversityService;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class UniversityEventService {

    private final UniversityService universityService;
    private final EventService eventService;

    public Events getCurrentEventOfUniversity(String university) {
        Optional<Long> currentEvent = universityService.getCurrentEvent(university);

        if (currentEvent.isPresent()) {
            return eventService.findById(currentEvent.get());
        }
        throw new EventException(ErrorCode.NOT_FOUND_CURRENT_EVENT);
    }
}

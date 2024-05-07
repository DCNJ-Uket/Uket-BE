package com.uket.domain.event.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.exception.EventException;
import com.uket.domain.event.repository.EventRepository;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.exception.UniversityException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class EventService {

    private final EventRepository eventRepository;

    public Events findById(Long eventId) {
        return eventRepository.findById(eventId)
                .orElseThrow(() -> new EventException(ErrorCode.NOT_FOUND_EVENT));
    }

    public String getUniversityName(Long eventId) {
        Events event = findById(eventId);
        University university = event.getUniversity();

        if (university == null) {
            throw new UniversityException(ErrorCode.NOT_FOUND_UNIVERSITY);
        }
        return university.getName();
    }
}

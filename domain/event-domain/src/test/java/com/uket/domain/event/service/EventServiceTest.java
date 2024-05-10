package com.uket.domain.event.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.exception.EventException;
import com.uket.domain.event.repository.EventRepository;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.exception.UniversityException;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class EventServiceTest {

    @InjectMocks
    EventService eventService;

    @Mock
    EventRepository eventRepository;

    @Test
    void 해당_축제가_존재하지_않는_경우_예외를_반환한다() {

        when(eventRepository.findById(any())).thenReturn(Optional.empty());

        assertThatThrownBy(() -> eventService.findById(1L))
                .isInstanceOf(EventException.class)
                .hasMessage(ErrorCode.NOT_FOUND_EVENT.getMessage());
    }

    @Test
    void 축제가_포함된_대학의_이름을_반환할_수_있다() {
        University university = University.builder()
                .name("건국대학교")
                .build();
        Events event = Events.builder()
                .id(1L)
                .university(university)
                .build();

        when(eventRepository.findById(any())).thenReturn(Optional.ofNullable(event));

        String universityName = eventService.getUniversityName(event.getId());
        assertThat(universityName).isEqualTo(university.getName());
    }

    @Test
    void 축제가_대학에_속해있지_않다면_예외를_반환한다() {
        Events event = Events.builder()
                .id(1L)
                .build();

        when(eventRepository.findById(any())).thenReturn(Optional.ofNullable(event));

        Long eventId = event.getId();
        assertThatThrownBy(() -> eventService.getUniversityName(eventId))
                .isInstanceOf(UniversityException.class)
                .hasMessage(ErrorCode.NOT_FOUND_UNIVERSITY.getMessage());
    }
}

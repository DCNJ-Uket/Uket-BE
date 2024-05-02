package com.uket.domain.event.service;

import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.exception.EventException;
import com.uket.domain.event.repository.EventRepository;
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
}

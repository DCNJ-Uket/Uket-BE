package com.uket.domain.event.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.uket.domain.event.dto.ShowDto;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.event.repository.ShowRepository;
import com.uket.domain.university.entity.University;
import java.time.LocalDateTime;
import java.util.List;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ShowServiceTest {

    @InjectMocks
    ShowService showService;

    @Mock
    ShowRepository showRepository;

    @Test
    void 공연을_조회할_수_있다() {
        University university = University.builder().id(1L).name("건국대학교").build();

        Events event = Events.builder().id(1L).university(university).build();

        ShowDto show = ShowDto.builder()
                .id(1L)
                .name("DAY1")
                .startDate(LocalDateTime.now())
                .endDate(LocalDateTime.now())
                .ticketingDate(LocalDateTime.now())
                .location("자양동")
                .totalTicketCount(1000)
                .build();

        when(showRepository.findByEventId(any())).thenReturn(List.of(show));

        Assertions.assertThat(showService.findByEventId(event.getId()))
                .hasSize(1)
                .contains(show);
    }
}

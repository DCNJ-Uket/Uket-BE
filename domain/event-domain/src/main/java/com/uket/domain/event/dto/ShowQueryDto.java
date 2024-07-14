package com.uket.domain.event.dto;

import com.uket.domain.event.entity.Shows;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import lombok.Builder;

@Builder
public record ShowQueryDto(
        Long id,
        String name,
        LocalDateTime startDate,
        LocalDateTime endDate,
        LocalDateTime ticketingDate,
        Integer totalTicketCount,
        String location
) {

    public static ShowQueryDto from(Shows show) {
        return ShowQueryDto.builder()
                .id(show.getId())
                .name(show.getName())
                .startDate(show.getStartDate())
                .endDate(show.getEndDate())
                .ticketingDate(show.getTicketingDate())
                .totalTicketCount(show.getTotalTicketCount())
                .location(show.getLocation())
                .build();
    }
}

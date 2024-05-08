package com.uket.domain.event.dto;

import com.uket.domain.event.entity.Shows;
import java.time.LocalDateTime;
import lombok.Builder;

@Builder
public record ShowDto(
        Long id,
        String name,
        LocalDateTime startDate,
        LocalDateTime endDate,
        LocalDateTime ticketingDate,
        Integer totalTicketCount,
        String location
) {

    public static ShowDto from(Shows show) {
        return ShowDto.builder()
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

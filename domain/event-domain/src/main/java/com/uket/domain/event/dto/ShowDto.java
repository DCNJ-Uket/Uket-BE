package com.uket.domain.event.dto;

import com.uket.domain.event.entity.Shows;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import lombok.Builder;

@Builder
public record ShowDto(
        Long id,
        String name,
        ZonedDateTime startDate,
        ZonedDateTime endDate,
        ZonedDateTime ticketingDate,
        Integer totalTicketCount,
        String location
) {

    public static ShowDto from(Shows show) {
        String zoneId = "Asia/Seoul";

        return ShowDto.builder()
                .id(show.getId())
                .name(show.getName())
                .startDate(ZonedDateTime.of(show.getStartDate(), ZoneId.of(zoneId)))
                .endDate(ZonedDateTime.of(show.getEndDate(), ZoneId.of(zoneId)))
                .ticketingDate(ZonedDateTime.of(show.getTicketingDate(), ZoneId.of(zoneId)))
                .totalTicketCount(show.getTotalTicketCount())
                .location(show.getLocation())
                .build();
    }
}

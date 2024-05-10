package com.uket.domain.event.dto;

import com.uket.domain.event.entity.Ticketing;
import com.uket.domain.event.enums.TicketingUserType;
import java.time.LocalDateTime;
import lombok.Builder;

@Builder
public record TicketingDto(
        Long ticketingId,
        TicketingUserType type,
        LocalDateTime startTime,
        LocalDateTime endTime,
        Integer reservedCount,
        Integer totalCount
) {
    public static TicketingDto from(Ticketing ticketing) {
        return TicketingDto.builder()
                .ticketingId(ticketing.getId())
                .type(ticketing.getType())
                .startTime(ticketing.getStartTime())
                .endTime(ticketing.getEndTime())
                .reservedCount(ticketing.getReservedCount())
                .totalCount(ticketing.getTotalCount())
                .build();
    }
}

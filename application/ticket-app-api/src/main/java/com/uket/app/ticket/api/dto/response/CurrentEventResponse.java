package com.uket.app.ticket.api.dto.response;

import com.uket.domain.event.entity.Events;
import java.time.LocalDate;
import lombok.Builder;

@Builder
public record CurrentEventResponse(
        String name,
        LocalDate startDate,
        LocalDate endDate
) {

    public static CurrentEventResponse from(Events event) {
        return CurrentEventResponse.builder()
                .name(event.getName())
                .startDate(event.getStartDate())
                .endDate(event.getEndDate())
                .build();
    }
}

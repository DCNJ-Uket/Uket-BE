package com.uket.app.ticket.api.dto.response;

import java.time.ZonedDateTime;
import lombok.Builder;

@Builder
public record ActiveUniversitiesResponse(
        Long id,
        String name,
        String logoUrl,
        ZonedDateTime startDateTime
){}

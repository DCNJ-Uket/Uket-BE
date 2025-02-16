package com.uket.app.ticket.api.dto.response;

import java.time.ZonedDateTime;
import com.uket.domain.university.dto.UniversityDto;
import com.uket.domain.university.entity.University;
import java.time.LocalDate;
import java.time.LocalDateTime;
import lombok.Builder;

@Builder
public record ActiveUniversitiesResponse(
        Long id,
        String name,
        String eventName,
        String logoUrl,
        ZonedDateTime startDateTime
) {
}

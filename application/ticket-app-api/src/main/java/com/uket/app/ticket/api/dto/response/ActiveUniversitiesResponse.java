package com.uket.app.ticket.api.dto.response;

import com.uket.domain.university.dto.UniversityDto;
import com.uket.domain.university.entity.University;
import java.time.LocalDate;
import java.time.LocalDateTime;
import lombok.Builder;

@Builder
public record ActiveUniversitiesResponse(
        Long id,
        String name,
        String logoUrl,
        LocalDateTime startDateTime
) {
    public static UniversityDto from(University university) {
        return UniversityDto.builder()
                .id(university.getId())
                .name(university.getName())
                .logoUrl(university.getLogoPath())
                .build();
    }
}

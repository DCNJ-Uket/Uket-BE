package com.uket.domain.university.dto;

import com.uket.domain.university.entity.University;
import java.time.LocalDate;
import lombok.Builder;

@Builder
public record UniversityDto(
        Long id,
        String name,
        String logoUrl,
        LocalDate startDate,
        LocalDate endDate
) {
    public static UniversityDto from(University university) {
        return UniversityDto.builder()
                .id(university.getId())
                .name(university.getName())
                .logoUrl(university.getLogoPath())
                .build();
    }
}

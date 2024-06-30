package com.uket.domain.university.dto;

import com.uket.domain.university.entity.University;
import lombok.Builder;

@Builder
public record UniversityDto(
        Long id,
        String name,
        String logoUrl
) {

    public static UniversityDto from(University university) {
        return UniversityDto.builder()
                .id(university.getId())
                .name(university.getName())
                .logoUrl(university.getLogoPath())
                .build();
    }

    public static UniversityDto updateLogoUrl(UniversityDto universityDto, String logoUrl) {
        return UniversityDto.builder()
                .id(universityDto.id())
                .name(universityDto.name())
                .logoUrl(logoUrl)
                .build();
    }
}

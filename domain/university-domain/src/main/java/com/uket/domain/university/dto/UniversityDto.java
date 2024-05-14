package com.uket.domain.university.dto;

import lombok.Builder;

@Builder
public record UniversityDto(
        Long id,
        String name,
        String logoUrl
) {

    public static UniversityDto updateLogoUrl(UniversityDto universityDto, String logoUrl) {
        return UniversityDto.builder()
                .id(universityDto.id())
                .name(universityDto.name())
                .logoUrl(logoUrl)
                .build();
    }
}

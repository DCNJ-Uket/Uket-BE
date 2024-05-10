package com.uket.app.ticket.api.dto.response;

import com.uket.domain.university.dto.UniversityDto;
import lombok.Builder;

@Builder
public record UniversityResponse(
        Long id,
        String name,
        String logoUrl
) {

    public static UniversityResponse of(UniversityDto universityDto, String logoUrl) {
        return UniversityResponse.builder()
                .id(universityDto.id())
                .name(universityDto.name())
                .logoUrl(logoUrl)
                .build();
    }
}

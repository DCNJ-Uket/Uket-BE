package com.uket.app.ticket.api.dto.response;

public record CertifiableUniversityResponse(
        Long universityId,
        String name
) {
    public static CertifiableUniversityResponse of(Long universityId, String name) {
        return new CertifiableUniversityResponse(universityId, name);
    }
}

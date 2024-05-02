package com.uket.domain.user.dto;

import lombok.Builder;

@Builder
public record CreateUserDetailsDto(
        String depositorName,
        String phoneNumber,
        String universityEmail,
        String studentMajor,
        String studentCode
) {

}

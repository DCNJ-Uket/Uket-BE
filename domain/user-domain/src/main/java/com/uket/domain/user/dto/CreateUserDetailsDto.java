package com.uket.domain.user.dto;

public record CreateUserDetailsDto(
        String depositorName,
        String phoneNumber,
        String studentMajor,
        String studentCode
) {

}

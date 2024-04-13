package com.uket.domain.user.dto;

import lombok.Builder;

@Builder
public record UserDto(
        Long memberId,
        String name,
        String role,
        Boolean isRegistered
) {

}

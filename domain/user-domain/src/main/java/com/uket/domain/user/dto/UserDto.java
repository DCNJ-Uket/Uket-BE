package com.uket.domain.user.dto;

import lombok.Builder;

@Builder
public record UserDto(
        Long userId,
        String name,
        String role,
        Boolean isRegistered
) {

}

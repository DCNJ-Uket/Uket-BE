package com.uket.app.domain.user;

import lombok.Builder;

@Builder
public record UserDto(
        Long userId,
        String name,
        String role,
        Boolean isRegistered
) {

}

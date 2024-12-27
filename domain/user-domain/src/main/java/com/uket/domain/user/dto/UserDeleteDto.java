package com.uket.domain.user.dto;

import com.uket.domain.user.entity.Users;
import lombok.Builder;

@Builder
public record UserDeleteDto(
    Long userId,
    String name
) {
    public static UserDeleteDto of(Users user) {
        return UserDeleteDto.builder()
            .userId(user.getId())
            .name(user.getName())
            .build();
    }
}

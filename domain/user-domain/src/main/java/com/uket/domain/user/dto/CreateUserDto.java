package com.uket.domain.user.dto;

import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import lombok.Builder;

@Builder
public record CreateUserDto(
        String name,
        String email,
        String profileImage,
        Platform platform,
        String platformId,
        UserRole role
) {

}

package com.uket.app.ticket.api.dto.response;

import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.user.entity.Users;
import lombok.Builder;

@Builder
public record AuthResponse(
        Long id,
        String name,
        String accessToken,
        String refreshToken,
        Boolean isRegistered
) {
    public static AuthResponse of(Users user, AuthToken authToken) {
        return AuthResponse.builder()
                .id(user.getId())
                .name(user.getName())
                .accessToken(authToken.accessToken())
                .refreshToken(authToken.refreshToken())
                .isRegistered(authToken.isRegistered())
                .build();
    }
}

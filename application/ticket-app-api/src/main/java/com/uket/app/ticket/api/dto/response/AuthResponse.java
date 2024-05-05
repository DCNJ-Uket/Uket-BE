package com.uket.app.ticket.api.dto.response;

import com.uket.domain.auth.dto.response.AuthToken;
import lombok.Builder;

@Builder
public record AuthResponse(
        Long id,
        String name,
        String accessToken,
        String refreshToken,
        Boolean isRegistered
) {
    public static AuthResponse of(Long userId, String userName,AuthToken authToken) {
        return AuthResponse.builder()
                .id(userId)
                .name(userName)
                .accessToken(authToken.accessToken())
                .refreshToken(authToken.refreshToken())
                .isRegistered(authToken.isRegistered())
                .build();
    }
}

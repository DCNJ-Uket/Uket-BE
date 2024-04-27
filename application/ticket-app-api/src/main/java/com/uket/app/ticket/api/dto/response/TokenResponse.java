package com.uket.app.ticket.api.dto.response;

import com.uket.domain.auth.dto.response.AuthToken;
import lombok.Builder;

@Builder
public record TokenResponse(
        String accessToken,
        String refreshToken,
        Boolean isRegistered
) {

    public static TokenResponse from(AuthToken authToken) {
        return TokenResponse.builder()
                .accessToken(authToken.accessToken())
                .refreshToken(authToken.refreshToken())
                .isRegistered(authToken.isRegistered())
                .build();
    }
}

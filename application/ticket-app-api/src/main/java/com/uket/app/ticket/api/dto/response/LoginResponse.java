package com.uket.app.ticket.api.dto.response;

import com.uket.domain.auth.dto.response.AuthToken;
import lombok.Builder;

@Builder
public record LoginResponse(
        String accessToken,
        String refreshToken,
        Boolean isRegistered
) {

    public static LoginResponse from(AuthToken authToken) {
        return LoginResponse.builder()
                .accessToken(authToken.accessToken())
                .refreshToken(authToken.refreshToken())
                .isRegistered(authToken.isRegistered())
                .build();
    }
}

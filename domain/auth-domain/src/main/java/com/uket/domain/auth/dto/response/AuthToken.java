package com.uket.domain.auth.dto.response;

public record AuthToken(
        String accessToken,
        String refreshToken
) {
    public static AuthToken of(String accessToken, String refreshToken) {
        return new AuthToken(accessToken, refreshToken);
    }
}

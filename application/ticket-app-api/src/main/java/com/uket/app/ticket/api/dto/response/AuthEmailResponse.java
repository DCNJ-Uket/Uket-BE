package com.uket.app.ticket.api.dto.response;

public record AuthEmailResponse(
        Boolean success,
        String email,
        Long expiration
) {

    public static AuthEmailResponse of(String email, Long expiration) {
        return new AuthEmailResponse(true, email, expiration);
    }
}

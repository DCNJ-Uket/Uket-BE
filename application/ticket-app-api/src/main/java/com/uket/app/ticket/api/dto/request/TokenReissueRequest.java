package com.uket.app.ticket.api.dto.request;

public record TokenReissueRequest(
        String accessToken,
        String refreshToken
) {

}

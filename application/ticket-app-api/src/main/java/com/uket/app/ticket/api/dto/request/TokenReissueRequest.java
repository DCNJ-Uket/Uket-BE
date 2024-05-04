package com.uket.app.ticket.api.dto.request;

import jakarta.validation.constraints.NotNull;

public record TokenReissueRequest(
        @NotNull(message = "accessToken 은 null 일 수 없습니다.")
        String accessToken,
        @NotNull(message = "refreshToken 은 null 일 수 없습니다.")
        String refreshToken
) {

}

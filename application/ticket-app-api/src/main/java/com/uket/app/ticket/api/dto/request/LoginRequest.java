package com.uket.app.ticket.api.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;

public record LoginRequest(
        @Schema(description = "인가 코드", example = "C95mSFwbWNVWUA1OCQ9mLSYFU5Dj7b5sFTgsy5lUNAAABjwtYyp")
        String code,
        @Schema(description = "Redirect URI", example = "http://localhost:8080/login/oauth2/code/kakao")
        String redirectUri
) {

}

package com.uket.domain.auth.dto.response.token;

public interface OAuth2TokenResponse {

    String getTokenType();

    String getAccessToken();

    String getScope();
}

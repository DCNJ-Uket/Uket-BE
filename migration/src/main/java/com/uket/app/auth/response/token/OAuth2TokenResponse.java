package com.uket.app.auth.response.token;

public interface OAuth2TokenResponse {

    String getTokenType();

    String getAccessToken();

    String getScope();
}

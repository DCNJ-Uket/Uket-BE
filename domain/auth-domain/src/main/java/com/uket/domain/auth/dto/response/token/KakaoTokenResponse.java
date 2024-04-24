package com.uket.domain.auth.dto.response.token;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.ToString;

@ToString(of = {"tokenType","accessToken","scope"})
public class KakaoTokenResponse implements OAuth2TokenResponse{
        @JsonProperty("token_type")
        String tokenType;
        @JsonProperty("access_token")
        String accessToken;
        @JsonProperty("expires_in")
        Long expiredIn;
        @JsonProperty("refresh_token")
        String refreshToken;
        @JsonProperty("refresh_token_expires_in")
        Long refreshTokenExpiresIn;
        @JsonProperty("scope")
        String scope;

        @Override
        public String getTokenType() {
                return this.tokenType;
        }

        @Override
        public String getAccessToken() {
                return this.accessToken;
        }

        @Override
        public String getScope() {
                return this.scope;
        }
}

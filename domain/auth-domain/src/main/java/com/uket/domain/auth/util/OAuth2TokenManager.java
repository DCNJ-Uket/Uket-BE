package com.uket.domain.auth.util;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.dto.response.token.KakaoTokenResponse;
import com.uket.domain.auth.dto.response.token.OAuth2TokenResponse;
import com.uket.domain.auth.exception.AuthException;
import com.uket.domain.auth.properties.AppProperties;
import com.uket.domain.user.enums.Platform;
import java.net.URI;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClient;
import org.springframework.web.util.UriBuilder;

@Component
@RequiredArgsConstructor
public class OAuth2TokenManager extends OAuth2Manager {

    private final AppProperties appProperties;

    public OAuth2TokenResponse getAccessToken(Platform platform, String redirectUri, String code) {

        if (platform == Platform.KAKAO) {
            return getKakaoTokenResponse(redirectUri, code);
        }
        throw new AuthException(ErrorCode.INVALID_PLATFORM);
    }

    private OAuth2TokenResponse getKakaoTokenResponse(String redirectUri, String code) {
        RestClient restClient = createRestClient(appProperties.kakao().tokenUri());

        return restClient
                .post()
                .uri(uriBuilder -> getKakaoTokenUri(redirectUri, code, uriBuilder))
                .header(HttpHeaders.CONTENT_TYPE, MEDIA_TYPE)
                .retrieve()
                .body(KakaoTokenResponse.class);
    }

    private URI getKakaoTokenUri(String redirectUri, String code, UriBuilder uriBuilder) {
        return uriBuilder
                .queryParam("grant_type", "authorization_code")
                .queryParam("redirect_uri", redirectUri)
                .queryParam("client_id", appProperties.kakao().clientId())
                .queryParam("client_secret", appProperties.kakao().clientSecret())
                .queryParam("code", code)
                .build();
    }
}

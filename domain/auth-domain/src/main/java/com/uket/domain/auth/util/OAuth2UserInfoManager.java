package com.uket.domain.auth.util;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.dto.response.userinfo.KakaoUserInfoResponse;
import com.uket.domain.auth.dto.response.userinfo.OAuth2UserInfoResponse;
import com.uket.domain.auth.dto.response.token.OAuth2TokenResponse;
import com.uket.domain.auth.exception.AuthException;
import com.uket.domain.auth.properties.AppProperties;
import com.uket.domain.user.enums.Platform;
import java.net.URI;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClient;
import org.springframework.web.util.UriBuilder;

@Component
@RequiredArgsConstructor
public class OAuth2UserInfoManager extends OAuth2Manager {

    private final AppProperties appProperties;

    public OAuth2UserInfoResponse getUserInfo(Platform platform,
            OAuth2TokenResponse tokenResponse) {
        if (platform == Platform.KAKAO) {
            return getKakaoUserInfoResponse(tokenResponse);
        }
        throw new AuthException(ErrorCode.INVALID_PLATFORM);
    }

    private OAuth2UserInfoResponse getKakaoUserInfoResponse(OAuth2TokenResponse tokenResponse) {
        RestClient restClient = createRestClient(appProperties.kakao().userInfoUri());
        String authorization = String.join(" ", tokenResponse.getTokenType(),
                tokenResponse.getAccessToken());

        Map<String, Object> response = requestUserInfoToKakao(restClient, authorization);

        if (response != null) {
            return new KakaoUserInfoResponse(response);
        }
        throw new AuthException(ErrorCode.FAIL_REQUEST_TO_OAUTH2);
    }

    private Map<String, Object> requestUserInfoToKakao(RestClient restClient,
            String authorization) {
        return restClient
                .post()
                .uri(this::getKakaoUserInfoUri)
                .header(HttpHeaders.AUTHORIZATION, authorization)
                .header(HttpHeaders.CONTENT_TYPE, MEDIA_TYPE)
                .retrieve()
                .body(new ParameterizedTypeReference<>() {
                });
    }

    private URI getKakaoUserInfoUri(UriBuilder uriBuilder) {
        return uriBuilder
                .queryParam("grant_type", "authorization_code")
                .queryParam("client_id", appProperties.kakao().clientId())
                .queryParam("client_secret", appProperties.kakao().clientSecret())
                .queryParam("property_keys", "[\"kakao_account.email\", \"kakao_account.name\"]")
                .build();
    }
}

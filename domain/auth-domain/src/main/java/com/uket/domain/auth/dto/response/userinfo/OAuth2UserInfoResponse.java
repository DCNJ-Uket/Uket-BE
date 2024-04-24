package com.uket.domain.auth.dto.response.userinfo;

public interface OAuth2UserInfoResponse {
    String getProvider();
    String getProviderId();
    String getEmail();
    String getName();
}

package com.uket.domain.auth.dto.response.userinfo;

import java.util.Map;

public class KakaoUserInfoResponse implements OAuth2UserInfoResponse {
    private final Map<String, Object> attribute;
    private final Map<String, Object> account;

    public KakaoUserInfoResponse(Map<String, Object> attribute) {
        this.attribute = attribute;
        this.account = (Map<String, Object>) attribute.get("kakao_account");
    }

    @Override
    public String getProvider() {
        return "kakao";
    }

    @Override
    public String getProviderId() {
        return attribute.get("id").toString();
    }

    @Override
    public String getEmail() {
        return account.get("email").toString();
    }

    @Override
    public String getName() {
        return account.get("name").toString();
    }
}

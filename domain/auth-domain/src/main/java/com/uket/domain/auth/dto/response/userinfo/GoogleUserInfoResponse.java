package com.uket.domain.auth.dto.response.userinfo;

import java.util.Map;
import lombok.RequiredArgsConstructor;


public class GoogleUserInfoResponse implements OAuth2UserInfoResponse {

    private final Map<String, Object> attribute;

    public GoogleUserInfoResponse(Map<String, Object> attribute) {
        this.attribute = attribute;
    }

    @Override
    public String getProvider() {
        return "google";
    }

    @Override
    public String getProviderId() {
        return attribute.get("id").toString();
    }

    @Override
    public String getEmail() {
        return attribute.get("email").toString();
    }

    @Override
    public String getName() {
        return attribute.get("name").toString();
    }
}

package com.uket.app.auth.response.userinfo;

public interface OAuth2UserInfoResponse {

    String getProvider();

    String getProviderId();

    String getEmail();

    String getName();

    String getProfileImage();
}

package com.uket.domain.auth.admin.dto;

public record AdminAuthToken(
        String accessToken
) {

    public static AdminAuthToken from(String accessToken) {
        return new AdminAuthToken(accessToken);
    }
}

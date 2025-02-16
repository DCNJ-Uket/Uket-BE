package com.uket.domain.auth.admin.dto;

public record AdminAuthToken(
        String accessToken,
        String name
) {

    public static AdminAuthToken from(String accessToken, String name) {
        return new AdminAuthToken(accessToken, name);
    }
}

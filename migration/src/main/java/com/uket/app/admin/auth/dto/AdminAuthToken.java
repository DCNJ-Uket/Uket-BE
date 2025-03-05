package com.uket.app.admin.auth.dto;

public record AdminAuthToken(
        String accessToken,
        String name
) {

    public static AdminAuthToken from(String accessToken, String name) {
        return new AdminAuthToken(accessToken, name);
    }
}

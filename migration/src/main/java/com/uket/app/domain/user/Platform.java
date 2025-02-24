package com.uket.app.domain.user;

import java.security.InvalidParameterException;

public enum Platform {
    KAKAO, GOOGLE;

    public static Platform fromString(String provider) {
        String platform = provider.toUpperCase();
        if (platform.equals("KAKAO")) {
            return KAKAO;
        }
        if (platform.equals("GOOGLE")) {
            return GOOGLE;
        }
        throw new InvalidParameterException();
    }
}

package com.uket.app.ticket.api.util;

import jakarta.servlet.http.Cookie;

public class CookieGenerator {

    public static Cookie createCookie(String key, String value, int maxAge) {

        Cookie cookie = new Cookie(key, value);
        cookie.setMaxAge(maxAge);
        cookie.setPath("/");
        cookie.setHttpOnly(true);
        //TODO: https 설정 이후 활성화
        //cookie.setSecure(true);

        return cookie;
    }
}

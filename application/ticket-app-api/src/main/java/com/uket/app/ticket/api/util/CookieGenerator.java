package com.uket.app.ticket.api.util;

import com.uket.jwtprovider.auth.constants.JwtValues;
import com.uket.jwtprovider.auth.properties.TokenProperties;
import java.time.Duration;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseCookie;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class CookieGenerator {

    private final TokenProperties tokenProperties;

    public ResponseCookie createCookie(String key, String value, int maxAge) {

        return ResponseCookie.from(key,value)
                .maxAge(Duration.ofMillis(maxAge))
                .domain(tokenProperties.domain())
                .path("/")
                .httpOnly(key.equals(JwtValues.JWT_PAYLOAD_VALUE_REFRESH))
                .secure(true)
                .build();
    }
}

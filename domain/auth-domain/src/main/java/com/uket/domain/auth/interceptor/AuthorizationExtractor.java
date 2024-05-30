package com.uket.domain.auth.interceptor;

import static com.uket.modules.jwt.constants.JwtValues.JWT_AUTHORIZATION_HEADER;
import static com.uket.modules.jwt.constants.JwtValues.JWT_AUTHORIZATION_VALUE_PREFIX;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.exception.AuthException;
import jakarta.servlet.http.HttpServletRequest;

public class AuthorizationExtractor {

    public static String extractAccessToken(HttpServletRequest request) {
        String accessToken = request.getHeader(JWT_AUTHORIZATION_HEADER);

        if (accessToken == null) {
            throw new AuthException(ErrorCode.AUTHENTICATION_FAILED);
        }
        return accessToken.replace(JWT_AUTHORIZATION_VALUE_PREFIX, "");
    }
}

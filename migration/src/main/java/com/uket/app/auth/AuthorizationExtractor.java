package com.uket.app.auth;

import static com.uket.modules.jwt.constants.JwtValues.JWT_AUTHORIZATION_HEADER;
import static com.uket.modules.jwt.constants.JwtValues.JWT_AUTHORIZATION_VALUE_PREFIX;

import com.uket.app.auth.exception.AuthException;
import com.uket.app.exception.ErrorCode;
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

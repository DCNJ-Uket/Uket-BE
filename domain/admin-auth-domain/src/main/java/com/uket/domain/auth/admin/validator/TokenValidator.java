package com.uket.domain.auth.admin.validator;


import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.admin.exception.AuthException;
import com.uket.modules.jwt.util.JwtAuthTokenUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class TokenValidator {

    private final JwtAuthTokenUtil jwtAuthTokenUtil;

    public void validateTokenSignature(String token) {
        if (Boolean.FALSE.equals(jwtAuthTokenUtil.isValidToken(token))) {
            throw new AuthException(ErrorCode.INVALID_TOKEN);
        }
    }

    public void validateExpiredToken(String token) {
        if (Boolean.TRUE.equals(jwtAuthTokenUtil.isExpired(token))) {
            throw new AuthException(ErrorCode.TOKEN_EXPIRED);
        }
    }

    public void validateTokenCategory(String category, String token) {
        String tokenCategory = jwtAuthTokenUtil.getCategory(token);
        if (Boolean.FALSE.equals(tokenCategory.equals(category))) {
            throw new AuthException(ErrorCode.NOT_MATCH_CATEGORY);
        }
    }

    public void checkNotExpiredToken(String token) {
        if (Boolean.FALSE.equals(jwtAuthTokenUtil.isExpired(token))) {
            throw new AuthException(ErrorCode.TOKEN_NOT_EXPIRED);
        }
    }

    public void validateRegistered(String accessToken) {
        if (Boolean.FALSE.equals(jwtAuthTokenUtil.isRegistered(accessToken))) {
            throw new AuthException(ErrorCode.NOT_REGISTERED_USER);
        }
    }
}

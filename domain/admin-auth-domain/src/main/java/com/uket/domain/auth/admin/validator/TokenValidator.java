package com.uket.domain.auth.admin.validator;


import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.admin.exception.AuthException;
import com.uket.domain.user.enums.UserRole;
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

    public void validateQRTokenSignature(String token) {
        if (Boolean.FALSE.equals(jwtAuthTokenUtil.isValidToken(token))) {
            throw new AuthException(ErrorCode.NOT_VALID_QR);
        }
    }

    public void validateExpiredToken(String token) {
        if (Boolean.TRUE.equals(jwtAuthTokenUtil.isExpired(token))) {
            throw new AuthException(ErrorCode.TOKEN_EXPIRED);
        }
    }

    public void validateExpiredQRToken(String token) {
        if (Boolean.TRUE.equals(jwtAuthTokenUtil.isExpired(token))) {
            throw new AuthException(ErrorCode.EXPIRED_QR_TOKEN);
        }
    }

    public void validateTokenCategory(String category, String token) {
        String tokenCategory = jwtAuthTokenUtil.getCategory(token);
        if (Boolean.FALSE.equals(tokenCategory.equals(category))) {
            throw new AuthException(ErrorCode.NOT_MATCH_CATEGORY);
        }
    }

    public void validateQRTokenCategory(String category, String token) {
        String tokenCategory = jwtAuthTokenUtil.getCategory(token);
        if (Boolean.FALSE.equals(tokenCategory.equals(category))) {
            throw new AuthException(ErrorCode.NOT_QR_TOKEN);
        }
    }

    public void validateRole(UserRole userRole, String accessToken) {
        String role = jwtAuthTokenUtil.getRole(accessToken);
        if (Boolean.FALSE.equals(userRole.equals(UserRole.valueOf(role)))) {
            throw new AuthException(ErrorCode.INVALID_ROLE);
        }
    }
}

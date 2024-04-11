package com.uket.domain.auth.service;

import static com.uket.jwtprovider.auth.constants.JwtValues.JWT_PAYLOAD_VALUE_REFRESH;

import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.auth.exception.ExpiredTokenException;
import com.uket.domain.auth.exception.InvalidRefreshTokenException;
import com.uket.domain.auth.exception.NotFoundRefreshTokenException;
import com.uket.jwtprovider.auth.JwtAuthTokenUtil;
import jakarta.servlet.http.Cookie;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class AuthService {

    private final JwtAuthTokenUtil jwtAuthTokenUtil;

    public AuthToken reissue(Cookie[] cookies) {
        String refreshToken = findRefreshToken(cookies);

        validateExpiredToken(refreshToken);
        validateTokenCategoryOfRefresh(refreshToken);

        return generateAuthToken(refreshToken);
    }

    private void validateExpiredToken(String refreshToken) {
        if (Boolean.TRUE.equals(jwtAuthTokenUtil.isExpired(refreshToken))) {
            throw new ExpiredTokenException();
        }
    }

    private void validateTokenCategoryOfRefresh(String token) {
        String category = jwtAuthTokenUtil.getCategory(token);
        if (Boolean.FALSE.equals(category.equals(JWT_PAYLOAD_VALUE_REFRESH))) {
            throw new InvalidRefreshTokenException();
        }
    }

    private AuthToken generateAuthToken(String refreshToken) {
        Long userId = jwtAuthTokenUtil.getId(refreshToken);
        String name = jwtAuthTokenUtil.getName(refreshToken);
        String role = jwtAuthTokenUtil.getRole(refreshToken);

        String newAccessToken = jwtAuthTokenUtil.createAccessToken(userId, name, role);
        String newRefreshToken = jwtAuthTokenUtil.createRefreshToken(userId, name, role);

        return AuthToken.of(newAccessToken, newRefreshToken);
    }

    private String findRefreshToken(Cookie[] cookies) {
        for (Cookie cookie : cookies) {
            if (cookie.getName().equals(JWT_PAYLOAD_VALUE_REFRESH)) {
                return cookie.getValue();
            }
        }
        throw new NotFoundRefreshTokenException();
    }
}
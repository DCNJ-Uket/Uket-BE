package com.uket.domain.auth.service;

import static com.uket.jwtprovider.auth.constants.JwtValues.JWT_PAYLOAD_VALUE_REFRESH;

import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.auth.dto.response.userinfo.OAuth2UserInfoResponse;
import com.uket.domain.auth.dto.response.token.OAuth2TokenResponse;
import com.uket.domain.auth.exception.NotFoundRefreshTokenException;
import com.uket.domain.auth.util.OAuth2TokenManager;
import com.uket.domain.auth.util.OAuth2UserInfoManager;
import com.uket.domain.auth.validator.TokenValidator;
import com.uket.domain.user.dto.UserDto;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.service.UserService;
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
    private final TokenValidator tokenValidator;
    private final OAuth2TokenManager oauth2TokenManager;
    private final OAuth2UserInfoManager oAuth2UserInfoManager;
    private final UserService userService;

    public AuthToken login(Platform platform, String redirectUri, String code) {

        OAuth2TokenResponse tokenResponse = oauth2TokenManager.getAccessToken(platform, redirectUri,
                code);
        OAuth2UserInfoResponse userInfo = oAuth2UserInfoManager.getUserInfo(platform, tokenResponse);

        return null;
    }

    public AuthToken reissue(Cookie[] cookies) {
        String refreshToken = findRefreshToken(cookies);

        tokenValidator.validateTokenSignature(refreshToken);
        tokenValidator.validateExpiredToken(refreshToken);
        tokenValidator.validateTokenCategory(JWT_PAYLOAD_VALUE_REFRESH, refreshToken);

        return generateAuthToken(UserDto.builder()
                .userId(jwtAuthTokenUtil.getId(refreshToken))
                .name(jwtAuthTokenUtil.getName(refreshToken))
                .role(jwtAuthTokenUtil.getRole(refreshToken))
                .isRegistered(true)
                .build());
    }


    private AuthToken generateAuthToken(UserDto userDto) {
        Long userId = userDto.userId();
        String name = userDto.name();
        String role = userDto.role();
        Boolean isRegistered = userDto.isRegistered();

        String newAccessToken = jwtAuthTokenUtil.createAccessToken(userId, name, role);
        String newRefreshToken = jwtAuthTokenUtil.createRefreshToken(userId, name, role);

        return AuthToken.of(newAccessToken, newRefreshToken, isRegistered);
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

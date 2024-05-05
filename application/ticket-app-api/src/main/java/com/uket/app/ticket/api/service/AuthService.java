package com.uket.app.ticket.api.service;

import static com.uket.modules.jwt.auth.constants.JwtValues.JWT_PAYLOAD_VALUE_REFRESH;

import com.uket.app.ticket.api.util.AuthTokenGenerator;
import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.auth.dto.response.userinfo.OAuth2UserInfoResponse;
import com.uket.domain.auth.dto.response.token.OAuth2TokenResponse;
import com.uket.domain.auth.util.OAuth2TokenManager;
import com.uket.domain.auth.util.OAuth2UserInfoManager;
import com.uket.domain.auth.validator.TokenValidator;
import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import com.uket.domain.user.service.UserService;
import com.uket.modules.jwt.auth.JwtAuthTokenUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class AuthService {

    private final JwtAuthTokenUtil jwtAuthTokenUtil;
    private final TokenValidator tokenValidator;
    private final OAuth2TokenManager oauth2TokenManager;
    private final OAuth2UserInfoManager oAuth2UserInfoManager;
    private final UserService userService;
    private final AuthTokenGenerator authTokenGenerator;

    @Transactional
    public AuthToken login(Platform platform, String redirectUri, String code) {

        OAuth2TokenResponse tokenResponse = oauth2TokenManager.getAccessToken(platform, redirectUri, code);
        OAuth2UserInfoResponse userInfo = oAuth2UserInfoManager.getUserInfo(platform, tokenResponse);

        Users newUser = userService.saveUser(generateCreateUserDto(userInfo));
        return authTokenGenerator.generateAuthToken(newUser);
    }

    public AuthToken reissue(String accessToken, String refreshToken) {

        tokenValidator.checkNotExpiredToken(accessToken);

        tokenValidator.validateExpiredToken(refreshToken);
        tokenValidator.validateTokenCategory(JWT_PAYLOAD_VALUE_REFRESH, refreshToken);
        tokenValidator.validateTokenSignature(refreshToken);

        Users findUser = userService.findById(jwtAuthTokenUtil.getId(refreshToken));

        return authTokenGenerator.generateAuthToken(findUser);
    }

    private CreateUserDto generateCreateUserDto(OAuth2UserInfoResponse userInfo) {
        return CreateUserDto.builder()
                .platform(Platform.fromString(userInfo.getProvider()))
                .platformId(userInfo.getProviderId())
                .email(userInfo.getEmail())
                .name(userInfo.getName())
                .profileImage(userInfo.getProfileImage())
                .role(UserRole.ROLE_USER)
                .build();
    }
}

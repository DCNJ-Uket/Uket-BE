package com.uket.domain.auth.service;

import static com.uket.modules.jwt.auth.constants.JwtValues.JWT_PAYLOAD_VALUE_ACCESS;
import static com.uket.modules.jwt.auth.constants.JwtValues.JWT_PAYLOAD_VALUE_REFRESH;

import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.auth.dto.response.userinfo.OAuth2UserInfoResponse;
import com.uket.domain.auth.dto.response.token.OAuth2TokenResponse;
import com.uket.domain.auth.util.OAuth2TokenManager;
import com.uket.domain.auth.util.OAuth2UserInfoManager;
import com.uket.domain.auth.validator.TokenValidator;
import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.dto.UserDto;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import com.uket.domain.user.service.UserService;
import com.uket.modules.jwt.auth.JwtAuthTokenUtil;
import java.util.Optional;
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

    @Transactional
    public AuthToken login(Platform platform, String redirectUri, String code) {

        OAuth2TokenResponse tokenResponse = oauth2TokenManager.getAccessToken(platform, redirectUri,
                code);
        OAuth2UserInfoResponse userInfo = oAuth2UserInfoManager.getUserInfo(platform, tokenResponse);

        Optional<Users> user = userService.findByPlatformAndPlatformId(
                userInfo.getProvider(), userInfo.getProviderId());

        if (user.isPresent()) {
            Users existUser = user.get();
            return generateAuthToken(generateUserDto(existUser));
        }
        Users newUser = userService.saveUser(generateCreateUserDto(userInfo));
        return generateAuthToken(generateUserDto(newUser));
    }

    public AuthToken reissue(String accessToken, String refreshToken) {

        tokenValidator.validateTokenSignature(accessToken);
        tokenValidator.checkNotExpiredToken(accessToken);
        tokenValidator.validateTokenCategory(JWT_PAYLOAD_VALUE_ACCESS, accessToken);

        tokenValidator.validateTokenSignature(refreshToken);
        tokenValidator.validateExpiredToken(refreshToken);
        tokenValidator.validateTokenCategory(JWT_PAYLOAD_VALUE_REFRESH, refreshToken);

        return generateAuthToken(UserDto.builder()
                .userId(jwtAuthTokenUtil.getId(accessToken))
                .name(jwtAuthTokenUtil.getName(accessToken))
                .role(jwtAuthTokenUtil.getRole(accessToken))
                .isRegistered(jwtAuthTokenUtil.isRegistered(accessToken))
                .build());
    }

    private CreateUserDto generateCreateUserDto(OAuth2UserInfoResponse userInfo) {
        return CreateUserDto.builder()
                .platform(Platform.fromString(userInfo.getProvider()))
                .platformId(userInfo.getProviderId())
                .email(userInfo.getEmail())
                .name(userInfo.getName())
                .role(UserRole.ROLE_USER)
                .build();
    }

    private UserDto generateUserDto(Users user) {
        return UserDto.builder()
                .userId(user.getId())
                .name(user.getName())
                .role(String.valueOf(user.getRole()))
                .isRegistered(user.getIsRegistered())
                .build();
    }

    private AuthToken generateAuthToken(UserDto userDto) {
        Long userId = userDto.userId();
        String name = userDto.name();
        String role = userDto.role();
        Boolean isRegistered = userDto.isRegistered();

        String newAccessToken = jwtAuthTokenUtil.createAccessToken(userId, name, role, isRegistered);
        String newRefreshToken = jwtAuthTokenUtil.createRefreshToken();

        return AuthToken.of(newAccessToken, newRefreshToken, isRegistered);
    }
}

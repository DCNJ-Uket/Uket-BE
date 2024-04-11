package com.uket.domain.auth.service;

import com.uket.domain.auth.domain.CustomOAuth2User;
import com.uket.domain.auth.dto.response.GoogleResponse;
import com.uket.domain.auth.dto.response.KakaoResponse;
import com.uket.domain.auth.dto.response.OAuth2Response;
import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.dto.UserDto;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.enums.UserRole;
import com.uket.domain.user.service.UserService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.oauth2.client.userinfo.DefaultOAuth2UserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
public class CustomOAuth2UserService extends DefaultOAuth2UserService {

    private final UserService userService;

    @Override
    public OAuth2User loadUser(OAuth2UserRequest userRequest) throws OAuth2AuthenticationException {
        OAuth2User oAuth2User = super.loadUser(userRequest);

        String registrationId = userRequest.getClientRegistration().getRegistrationId();
        OAuth2Response oAuth2Response;

        if (registrationId.equals("kakao")) {
            oAuth2Response = new
                    KakaoResponse(oAuth2User.getAttributes());
        } else if (registrationId.equals("google")) {
            oAuth2Response = new GoogleResponse(oAuth2User.getAttributes());
        } else {
            return null;
        }

        Users savedUser = userService.saveUser(generateCreateUserDto(oAuth2Response));

        return new CustomOAuth2User(generateUserDto(oAuth2Response, savedUser.getId()));
    }

    private static UserDto generateUserDto(OAuth2Response oAuth2Response, Long memberId) {
        return UserDto.builder()
                .role(String.valueOf(UserRole.ROLE_USER))
                .name(oAuth2Response.getName())
                .memberId(memberId)
                .build();
    }

    private static CreateUserDto generateCreateUserDto(OAuth2Response oAuth2Response) {
        Platform platform = Platform.fromString(oAuth2Response.getProvider());

        return CreateUserDto.builder()
                .email(oAuth2Response.getEmail())
                .name(oAuth2Response.getName())
                .platform(platform)
                .platformId(oAuth2Response.getProviderId())
                .role(UserRole.ROLE_USER)
                .build();
    }
}

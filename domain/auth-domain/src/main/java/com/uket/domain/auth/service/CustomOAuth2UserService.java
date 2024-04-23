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
import com.uket.domain.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
public class CustomOAuth2UserService implements UserDetailsService {

    private final UserRepository userRepository;

    @Override
    public UserDetails loadUserByUsername(String memberId) throws UsernameNotFoundException {
        Users user = userRepository.findById(Long.parseLong(memberId))
                .orElseThrow(() -> new UsernameNotFoundException("유저를 찾을 수 없습니다."));
        return new CustomOAuth2User(generateUserDto(user));
    }

    private static UserDto generateUserDto(Users user) {
        return UserDto.builder()
                .role(String.valueOf(UserRole.ROLE_USER))
                .name(user.getName())
                .isRegistered(user.getIsRegistered())
                .memberId(user.getId())
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

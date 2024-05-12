package com.uket.app.ticket.api.util;

import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.user.dto.UserDto;
import com.uket.domain.user.entity.Users;
import com.uket.modules.jwt.util.JwtAuthTokenUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class AuthTokenGenerator {

    private final JwtAuthTokenUtil jwtAuthTokenUtil;

    public AuthToken generateAuthToken(Users user) {
        UserDto userDto = generateUserDto(user);

        Long userId = userDto.userId();
        String name = userDto.name();
        String role = userDto.role();
        Boolean isRegistered = userDto.isRegistered();

        String newAccessToken = jwtAuthTokenUtil.createAccessToken(userId, name, role, isRegistered);
        String newRefreshToken = jwtAuthTokenUtil.createRefreshToken(userId);

        return AuthToken.of(newAccessToken, newRefreshToken, isRegistered);
    }

    private UserDto generateUserDto(Users user) {
        return UserDto.builder()
                .userId(user.getId())
                .name(user.getName())
                .role(String.valueOf(user.getRole()))
                .isRegistered(user.getIsRegistered())
                .build();
    }
}

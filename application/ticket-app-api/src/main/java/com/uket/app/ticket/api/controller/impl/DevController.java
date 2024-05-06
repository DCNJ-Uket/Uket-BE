package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.DevApi;
import com.uket.app.ticket.api.dto.response.TokenResponse;
import com.uket.app.ticket.api.service.UserRegisterService;
import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.user.dto.CreateUserDetailsDto;
import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.dto.UserDto;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import com.uket.domain.user.service.UserService;
import com.uket.modules.jwt.auth.JwtAuthTokenUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class DevController implements DevApi {

    private final JwtAuthTokenUtil jwtAuthTokenUtil;
    private final UserService userService;
    private final UserRegisterService userRegisterService;

    @Override
    public ResponseEntity<String> test(Long userId) {
        return ResponseEntity.ok("userId: " + userId);
    }

    @Override
    public ResponseEntity<TokenResponse> getToken() {

        Users user = userService.saveUser(generateCreateUserDto());
        userRegisterService.register(user.getId(),generateCreateUserDetailsDto(),"건국대학교");

        Users findUser = userService.findById(user.getId());
        UserDto userDto = generateUserDto(findUser);

        Boolean isRegistered = userDto.isRegistered();

        String newAccessToken = jwtAuthTokenUtil.createAccessToken(
                userDto.userId(),
                userDto.name(),
                userDto.role(),
                isRegistered);
        String newRefreshToken = jwtAuthTokenUtil.createRefreshToken(userDto.userId());

        AuthToken authToken = AuthToken.of(newAccessToken, newRefreshToken, isRegistered);
        return ResponseEntity.ok(TokenResponse.from(authToken));
    }

    private UserDto generateUserDto(Users user) {
        return UserDto.builder()
                .userId(user.getId())
                .name(user.getName())
                .role(String.valueOf(user.getRole()))
                .isRegistered(user.getIsRegistered())
                .build();
    }

    private CreateUserDto generateCreateUserDto() {
        return CreateUserDto.builder()
                .platform(Platform.KAKAO)
                .platformId("1234")
                .email("abc@naver.com")
                .name("테스트")
                .role(UserRole.ROLE_USER)
                .build();
    }

    private CreateUserDetailsDto generateCreateUserDetailsDto() {
        return CreateUserDetailsDto.builder()
                .depositorName("홍길동")
                .universityEmail("abc@konkuk.ac.kr")
                .phoneNumber("01012341234")
                .studentMajor("컴퓨터공학부")
                .studentCode("202411032")
                .build();
    }
}

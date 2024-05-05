package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.UserApi;
import com.uket.app.ticket.api.dto.request.UserRegisterRequest;
import com.uket.app.ticket.api.dto.response.AuthResponse;
import com.uket.app.ticket.api.service.UserRegisterService;
import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.user.dto.CreateUserDetailsDto;
import com.uket.modules.jwt.auth.JwtAuthTokenUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Slf4j
@Controller
@RequiredArgsConstructor
public class UserController implements UserApi {

    private final UserRegisterService userRegisterService;
    private final JwtAuthTokenUtil jwtAuthTokenUtil;

    @Override
    public ResponseEntity<AuthResponse> register(Long userId, UserRegisterRequest request) {

        CreateUserDetailsDto createUserDetailsDto = generateCreateUserDetailsDto(request);

        AuthToken authToken = userRegisterService.register(userId, createUserDetailsDto, request.university());
        String userName = jwtAuthTokenUtil.getName(authToken.accessToken());

        AuthResponse response = AuthResponse.of(userId,userName,authToken);
        return ResponseEntity.ok(response);
    }

    private CreateUserDetailsDto generateCreateUserDetailsDto(UserRegisterRequest request) {
        return CreateUserDetailsDto.builder()
                .depositorName(request.depositorName())
                .phoneNumber(request.phoneNumber())
                .universityEmail(request.universityEmail())
                .studentMajor(request.studentMajor())
                .studentCode(request.studentCode())
                .build();
    }
}

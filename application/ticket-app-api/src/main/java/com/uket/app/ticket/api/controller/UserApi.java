package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.request.UserRegisterRequest;
import com.uket.app.ticket.api.dto.response.AuthResponse;
import com.uket.domain.auth.config.userid.LoginUserId;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "유저 API", description = "유저 관련 API")
@RestController
@SecurityRequirement(name = "JWT")
@RequestMapping("/api/v1/users")
public interface UserApi {

    @Operation(summary = "회원가입", description = "회원가입을 진행합니다.")
    @PostMapping("/register")
    ResponseEntity<AuthResponse> register(
            @Parameter(hidden = true)
            @LoginUserId Long userId,

            @Valid
            @RequestBody UserRegisterRequest request
    );

}

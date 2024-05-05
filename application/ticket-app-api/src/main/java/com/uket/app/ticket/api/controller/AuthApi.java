package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.request.LoginRequest;
import com.uket.app.ticket.api.dto.request.TokenReissueRequest;
import com.uket.app.ticket.api.dto.response.AuthResponse;
import com.uket.app.ticket.api.dto.response.TokenResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "인증 API", description = "인증(로그인) 관련 API")
@RestController
@RequestMapping("/api/v1/auth")
public interface AuthApi {

    @Operation(summary = "소셜 로그인", description = "소셜 로그인을 진행합니다.")
    @PostMapping("/login/{provider}")
    ResponseEntity<AuthResponse> login(
            @Valid
            @RequestBody LoginRequest request,
            @Parameter(example = "kakao", description = "oAuth 제공자 이름")
            @PathVariable("provider") String provider
    );

    @Operation(summary = "토큰 재발행", description = "리프레시 토큰으로 새로은 토큰을 발행합니다.")
    @PostMapping(value = "/reissue")
    ResponseEntity<TokenResponse> reissue(
            @Valid
            @RequestBody TokenReissueRequest request
    );
}

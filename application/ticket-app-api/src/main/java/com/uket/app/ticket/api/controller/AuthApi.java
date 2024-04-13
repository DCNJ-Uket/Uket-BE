package com.uket.app.ticket.api.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "인증 API", description = "인증(로그인) 관련 API")
@RestController
@RequestMapping("/api/v1/auth")
public interface AuthApi {

    @Operation(summary = "토큰 재발행", description = "리프레시 토큰으로 새로은 토큰을 발행합니다.")
    @PostMapping(value = "/reissue")
    @Parameters
    ResponseEntity<Void> reissue(
            HttpServletRequest request,
            HttpServletResponse response
    );
}

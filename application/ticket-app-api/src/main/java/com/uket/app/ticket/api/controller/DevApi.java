package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.response.TokenResponse;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/dev")
public interface DevApi {

    @GetMapping("/token")
    @Operation(summary = "토큰 강제 발행", description = "토큰을 발급합니다.")
    ResponseEntity<TokenResponse> getToken();
}

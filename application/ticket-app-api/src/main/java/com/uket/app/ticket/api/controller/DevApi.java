package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.config.userid.LoginUserId;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/dev")
public interface DevApi {

    @GetMapping
    @Operation(summary = "테스트용 api", description = "테스트를 진행합니다.")
    @SecurityRequirement(name = "JWT")
    ResponseEntity<String> test(
            @LoginUserId Long userId
    );
}

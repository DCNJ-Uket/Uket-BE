package com.uket.app.admin.api.controller;

import com.uket.app.admin.api.dto.request.EmailLoginRequest;
import com.uket.core.dto.response.ErrorResponse;
import com.uket.domain.auth.admin.dto.AdminAuthToken;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "어드민용 인증 관련 API", description = "어드민용 인증 관련 API")
@RestController
@RequestMapping("/admin/v1/auth")
@ApiResponse(responseCode = "200", description = "OK")
public interface AuthApi {

    @Operation(summary = "어드민 로그인", description = "어드민 로그인을 진행합니다.")
    @ApiResponse(responseCode = "400", description = "BAD REQUEST", content = @Content(
            mediaType = "application/json",
            examples = {
            }, schema = @Schema(implementation = ErrorResponse.class)))
    @PostMapping("/login")
    ResponseEntity<AdminAuthToken> login(
            @Valid
            @RequestBody EmailLoginRequest request
    );
}

package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.request.EmailRequest;
import com.uket.domain.auth.config.userid.LoginUserId;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "티켓 API", description = "티켓 관련 API")
@RestController
@RequestMapping("/api/v1/email")
@SecurityRequirement(name = "JWT")
@ApiResponse(responseCode = "200", description = "OK")
public interface EmailApi {

    @PostMapping("/send")
    @Operation(summary = "이메일 전송 요청 API", description = "이메일 인증 요청을 할 수 있습니다.")
    ResponseEntity<?> sendEmail(
            @Parameter(hidden = true)
            @LoginUserId
            Long userId,

            @Valid
            @RequestBody
            EmailRequest request
    );
}

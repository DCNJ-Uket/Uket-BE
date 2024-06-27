package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.request.AuthEmailRequest;
import com.uket.app.ticket.api.dto.response.AuthEmailResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "이메일 API", description = "이메일 관련 API")
@RestController
@RequestMapping("/api/v1/email")
@ApiResponse(responseCode = "200", description = "OK")
public interface EmailApi {

    @PostMapping("/send")
    @Operation(summary = "이메일 전송 요청 API", description = "이메일 인증 요청을 할 수 있습니다.")
    ResponseEntity<AuthEmailResponse> sendEmail(
            @Valid
            @RequestBody
            AuthEmailRequest request
    );
}

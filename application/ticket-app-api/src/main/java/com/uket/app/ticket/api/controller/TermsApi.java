package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.request.TermsAgreementRequest;
import com.uket.app.ticket.api.dto.response.ListResponse;
import com.uket.app.ticket.api.dto.response.TermsAgreementResponse;
import com.uket.app.ticket.api.dto.response.TermsResponse;
import com.uket.domain.auth.config.userid.LoginUserId;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.List;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "약관 API", description = "약관 관련 API")
@RestController
@SecurityRequirement(name = "JWT")
@ApiResponse(responseCode = "200", description = "OK")
public interface TermsApi {

    @GetMapping("/api/v1/terms")
    @Operation(summary = "약관 목록 조회 API", description = "회원가입 시 필요한 약관 목록을 조회할 수 있습니다.")
    ResponseEntity<ListResponse<TermsResponse>> getTerms(
            @LoginUserId
            @Parameter(hidden = true)
            Long userId
    );

    @PostMapping("/api/v1/terms/agreement")
    @Operation(summary = "약관 동의 API", description = "회원가입 시 약관 동의를 할 수 있습니다.")
    ResponseEntity<ListResponse<TermsAgreementResponse>> agreeTerms(
            @LoginUserId
            @Parameter(hidden = true)
            Long userId,

            @RequestBody
            List<TermsAgreementRequest> requests
    );
}

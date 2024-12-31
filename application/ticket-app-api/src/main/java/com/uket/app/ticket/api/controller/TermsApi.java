package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.request.TermsAgreementRequest;
import com.uket.app.ticket.api.dto.response.ListResponse;
import com.uket.app.ticket.api.dto.response.TermsAgreementResponse;
import com.uket.app.ticket.api.dto.response.TermsResponse;
import com.uket.core.dto.response.ErrorResponse;
import com.uket.domain.auth.config.userid.LoginUserId;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
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
    @ApiResponse(responseCode = "404", description = "NOT FOUND", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "TE0001", description = "약관 id에 해당하는 약관을 찾을 수 없습니다.",
                            value = """
                                    {"code": "TE0001", "message": "약관을 찾을 수 없습니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    @ApiResponse(responseCode = "400", description = "BAD REQUEST", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "TE0002", description = "필수 문서에 대해 동의가 되지 않은 경우 발생합니다.",
                            value = """
                                    {"code": "TE0002", "message": "필수 문서는 동의가 필수입니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    ResponseEntity<ListResponse<TermsAgreementResponse>> agreeTerms(
            @LoginUserId
            @Parameter(hidden = true)
            Long userId,

            @RequestBody
            List<TermsAgreementRequest> requests
    );
}

package com.uket.app.admin.api.controller;

import com.uket.app.admin.api.dto.request.EmailLoginRequest;
import com.uket.app.admin.api.dto.request.EmailRegisterRequest;
import com.uket.app.admin.api.dto.response.AdminRegisterResponse;
import com.uket.core.dto.response.ErrorResponse;
import com.uket.domain.auth.admin.dto.AdminAuthToken;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
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
    @ApiResponse(responseCode = "404", description = "NOT FOUND", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "AD0001", description = "이메일을 찾을 수 없는 경우 발생합니다.",
                            value = """
                                    {"code": "AD0001", "message": "어드민의 이메일을 찾을 수 없습니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    @ApiResponse(responseCode = "400", description = "BAD REQUEST", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "AD0002", description = "비밀번호를 잘못 입력한 경우 발생합니다.",
                            value = """
                                    {"code": "AD0002", "message": "올바르지 않은 비밀번호입니다."}
                                    """
                    ),
                    @ExampleObject(name = "AD0004", description = "인가되지 않은 어드민인 경우 발생합니다. 문의가 필요합니다.",
                            value = """
                                    {"code": "AD0004", "message": "인가되지 않은 어드민입니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    @PostMapping("/login")
    ResponseEntity<AdminAuthToken> login(
            @Valid
            @RequestBody EmailLoginRequest request
    );

    @Operation(summary = "어드민 회원가입", description = "어드민 회원가입을 진행합니다.")
    @ApiResponse(responseCode = "400", description = "BAD REQUEST", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "AD0003", description = "이미 가입된 어드민인 경우 발생합니다.",
                            value = """
                                    {"code": "AD0003", "message": "이미 가입된 어드민입니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    @PostMapping("/register")
    ResponseEntity<AdminRegisterResponse> register(
            @Valid
            @RequestBody EmailRegisterRequest request
    );
}

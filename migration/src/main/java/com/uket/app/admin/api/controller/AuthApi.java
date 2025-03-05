package com.uket.app.admin.api.controller;

import com.uket.app.admin.api.dto.request.AdministratorRegisterRequest;
import com.uket.app.admin.api.dto.request.EmailLoginRequest;
import com.uket.app.admin.api.dto.request.EmailRegisterRequest;
import com.uket.app.admin.api.dto.response.ActiveOrganizationsResponse;
import com.uket.app.admin.api.dto.response.AdminRegisterResponse;
import com.uket.app.admin.api.dto.response.ListResponse;
import com.uket.app.dto.response.ErrorResponse;
import com.uket.app.admin.auth.dto.AdminAuthToken;
import com.uket.domain.auth.config.userid.LoginUserId;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.mail.MessagingException;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
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

    @Operation(summary = "어드민 계정 소속 조회", description = "어드민 계정의 소속을 조회합니다.")
    @GetMapping("/get/organizations")
    ResponseEntity<ListResponse<ActiveOrganizationsResponse>> getOrganizations();

    @Operation(summary = "관리자 어드민 계정 추가", description = "관리자가 비밀번호를 제외하고 어드민 계정을 추가 및 이메일 발송을 진행해 비밀번호를 등록할 수 있도록 합니다.")
    @PostMapping("/administrator/register")
    ResponseEntity<AdminRegisterResponse> registerWithOutPassword(
        @Valid
        @RequestBody AdministratorRegisterRequest request
    ) throws MessagingException;

    @Operation(summary = "관리자 어드민 계정 삭제", description = "관리자가 어드민 계정을 삭제합니다.")
    @PostMapping("/administrator/register/{deleteUserId}")
    ResponseEntity<Boolean> delete(
        @Parameter(hidden = true)
        @LoginUserId
        Long userId,
        @PathVariable("deleteUserId") Long deleteUserId
    );
}

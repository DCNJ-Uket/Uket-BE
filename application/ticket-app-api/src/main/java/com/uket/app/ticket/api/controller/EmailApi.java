package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.request.AuthCodeRequest;
import com.uket.app.ticket.api.dto.request.AuthEmailRequest;
import com.uket.app.ticket.api.dto.response.AuthEmailResponse;
import com.uket.core.dto.response.ErrorResponse;
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

@Tag(name = "이메일 API", description = "이메일 관련 API")
@RestController
@RequestMapping("/api/v1/email")
@ApiResponse(responseCode = "200", description = "OK")
public interface EmailApi {

    @PostMapping("/send")
    @ApiResponse(responseCode = "400", description = "BAD REQUEST", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "US0002", description = "이미 가입된 사용자의 이메일로 메일 전송 요청하는 경우 발생합니다.",
                            value = """
                                    {"code": "US0002", "message": "이미 가입된 사용자입니다."}
                                    """
                    ),
                    @ExampleObject(name = "AU0010", description = "이메일 전송이 실패한 경우 발생합니다.",
                            value = """
                                    {"code": "AU0010", "message": "이메일 전송에 실패했습니다."}
                                    """
                    ),
                    @ExampleObject(name = "UN0002", description = "대학 이메일 정보가 잘못되었습니다.",
                            value = """
                                    {"code": "UN0002", "message": "대학 이메일 정보가 잘못되었습니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    @ApiResponse(responseCode = "404", description = "NOT FOUND", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "UN0001", description = "universityId가 잘못된 경우 발생합니다.",
                            value = """
                                    {"code": "UN0001", "message": "해당 대학을 찾을 수 없습니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    @Operation(summary = "이메일 전송 요청 API", description = "이메일 인증 요청을 할 수 있습니다.")
    ResponseEntity<AuthEmailResponse> sendEmail(
            @Valid
            @RequestBody
            AuthEmailRequest request
    );

    @PostMapping("/verify")
    @ApiResponse(responseCode = "400", description = "BAD REQUEST", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "US0002", description = "이미 가입된 사용자의 이메일로 인증요청을 하는 경우 발생합니다.",
                            value = """
                                    {"code": "US0002", "message": "이미 가입된 사용자입니다."}
                                    """
                    ),
                    @ExampleObject(name = "UN0002", description = "대학 이메일 정보가 잘못되었습니다.",
                            value = """
                                    {"code": "UN0002", "message": "대학 이메일 정보가 잘못되었습니다."}
                                    """
                    ),
                    @ExampleObject(name = "AU0011", description = "기간이 만료된 인증 코드를 입력한 경우 발생합니다.",
                            value = """
                                    {"code": "AU0011", "message": "유효하지 않은 인증 코드입니다."}
                                    """
                    ),
                    @ExampleObject(name = "AU0012", description = "사용자가 발급받은 인증 코드가 아닌 경우 발생합니다.",
                            value = """
                                    {"code": "AU0012", "message": "올바르지 않은 인증 코드입니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    @ApiResponse(responseCode = "404", description = "NOT FOUND", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "UN0001", description = "universityId가 잘못된 경우 발생합니다.",
                            value = """
                                    {"code": "UN0001", "message": "해당 대학을 찾을 수 없습니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    @Operation(summary = "인증 코드 확인 API", description = "이메일 인증 코드를 확인 할 수 있습니다.")
    ResponseEntity<Void> verifyEmail(
            @Valid
            @RequestBody
            AuthCodeRequest request
    );
}

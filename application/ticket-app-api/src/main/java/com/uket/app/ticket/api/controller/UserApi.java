package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.request.UserRegisterRequest;
import com.uket.app.ticket.api.dto.response.AuthResponse;
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
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "유저 API", description = "유저 관련 API")
@RestController
@SecurityRequirement(name = "JWT")
@RequestMapping("/api/v1/users")
@ApiResponse(responseCode = "200", description = "OK")
public interface UserApi {

    @PostMapping("/register")
    @Operation(summary = "회원가입", description = "회원가입을 진행합니다.")
    @ApiResponse(responseCode = "400", description = "BAD REQUEST", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "UN0002",
                            description = "사용자가 선택한 대학의 이메일과 입력한 이메일의 prefix가 맞지 않는 경우 발생합니다.",
                            value = """
                                    {"code": "UN0002", "message": "대학 이메일 정보가 잘못되었습니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    @ApiResponse(responseCode = "404", description = "NOT FOUND", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "UN0001", description = "서버에서 대학 default 값 설정이 잘못된 경우 발생합니다.",
                            value = """
                                    {"code": "UN0001", "message": "해당 대학을 찾을 수 없습니다."}
                                    """
                    ),
                    @ExampleObject(name = "US0001", description = "토큰에 담긴 UserId에 대한 사용자를 찾을 수 없을 때 발생합니다.",
                            value = """
                                    {"code": "US0001", "message": "해당 사용자를 찾을 수 없습니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    ResponseEntity<AuthResponse> register(
            @Parameter(hidden = true)
            @LoginUserId Long userId,

            @Valid
            @RequestBody UserRegisterRequest request
    );
}

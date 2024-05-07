package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.request.LoginRequest;
import com.uket.app.ticket.api.dto.request.TokenReissueRequest;
import com.uket.app.ticket.api.dto.response.AuthResponse;
import com.uket.app.ticket.api.dto.response.TokenResponse;
import com.uket.core.dto.response.ErrorResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "인증 API", description = "인증(로그인) 관련 API")
@RestController
@RequestMapping("/api/v1/auth")
public interface AuthApi {

    @Operation(summary = "소셜 로그인", description = "소셜 로그인을 진행합니다.")
    @ApiResponse(responseCode = "400", description = "BAD REQUEST", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "CM0002", description = "잘못된 플랫폼을 입력할 시 발생합니다.",
                            value = """
                            {"code": "CM0002", "message": "유효하지 않은 입력입니다."}
                            """
                    ),
                    @ExampleObject(name = "AU0006", description = "지원하지 않는 플랫폼을 입력할 시 발생합니다.",
                            value = """
                            {"code": "AU0006", "message": "유효하지 않은 플랫폼입니다."}
                            """
                    ),
                    @ExampleObject(name = "AU0002", description = "OAuth2 서버와 통신이 실패할 경우 발생합니다.",
                            value = """
                            {"code": "AU0002", "message": "OAuth2 요청이 실패했습니다."}
                            """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    @PostMapping("/login/{provider}")
    ResponseEntity<AuthResponse> login(
            @Valid
            @RequestBody LoginRequest request,
            @Parameter(example = "kakao", description = "oAuth 제공자 이름")
            @PathVariable("provider") String provider
    );

    @Operation(summary = "토큰 재발행", description = "리프레시 토큰으로 새로은 토큰을 발행합니다.")
    @PostMapping(value = "/reissue")
    ResponseEntity<TokenResponse> reissue(
            @Valid
            @RequestBody TokenReissueRequest request
    );
}

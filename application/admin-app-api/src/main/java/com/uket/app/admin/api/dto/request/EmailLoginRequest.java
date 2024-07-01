package com.uket.app.admin.api.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotNull;

public record EmailLoginRequest(
        @Schema(description = "어드민 이메일", example = "abc1234@naver.com")
        @Email(message = "이메일 형식이 올바르지 않습니다.")
        String email,
        @Schema(description = "어드민 비밀번호", example = "qwer1234")
        @NotNull(message = "비밀번호는 null 일 수 없습니다.")
        String password
) {

}

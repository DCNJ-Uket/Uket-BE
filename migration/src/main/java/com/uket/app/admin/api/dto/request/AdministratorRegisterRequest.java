package com.uket.app.admin.api.dto.request;

import com.uket.app.domain.user.AdminRole;
import com.uket.domain.university.entity.University;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotNull;

public record AdministratorRegisterRequest(

    @Schema(description = "어드민 이름", example = "admin")
    @NotNull(message = "어드민 이름이 null 일 수 없습니다.")
    String name,
    @Schema(description = "어드민 이메일", example = "abc1234@naver.com")
    @Email(message = "이메일 형식이 올바르지 않습니다.")
    String email,
    @Schema(description = "어드민 소속", example = "소리터")
    @NotNull
    University organization,

    @Schema(description = "어드민 권한", example = "ADMINISTRATOR")
    @NotNull
    AdminRole role
) {

}

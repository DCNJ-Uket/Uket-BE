package com.uket.app.ticket.api.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Email;

public record AuthEmailRequest(
        @Email
        @Schema(description = "인증하고자 하는 이메일, ", example = "abc1234@konkuk.ac.kr")
        String email,

        @Schema(description = "인증 하고자 하는 대학 ID(건국대학교의 경우 2)", example = "2")
        Long universityId
) {

}

package com.uket.app.admin.api.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;

public record PhoneNumberRequest(
    @NotBlank
    @Pattern(regexp = "\\d{10,11}", message = "전화번호는 10자리 또는 11자리 숫자여야 합니다.")
    @Schema(description = "사용자의 전화번호", example = "01012345678")
    String phoneNumber
) {

}

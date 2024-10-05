package com.uket.app.admin.api.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;

public record UserNameRequest(
    @NotBlank
    @Schema(description = "검색하려는 사용자 이름", example = "곽민재")
    String userName
) {

}

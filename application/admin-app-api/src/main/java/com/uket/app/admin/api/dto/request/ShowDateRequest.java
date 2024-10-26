package com.uket.app.admin.api.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;

import java.time.LocalDateTime;

public record ShowDateRequest(
    @NotNull
    @Schema(description = "공연 날짜", example = "2024-09-05T00:00:00")
    LocalDateTime showDate
) {

}

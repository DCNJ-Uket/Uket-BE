package com.uket.app.admin.api.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import java.sql.Timestamp;
import java.time.LocalDateTime;

public record ModifiedAtRequest(
    @NotNull
    @Schema(description = "티켓 수정 일자", example = "2024-10-10T14:30:00")
    LocalDateTime modifiedAt
) {
    public Timestamp toTimestamp() {
        return Timestamp.valueOf(modifiedAt.withNano(0));
    }
}

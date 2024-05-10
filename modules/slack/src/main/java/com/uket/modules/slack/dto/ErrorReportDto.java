package com.uket.modules.slack.dto;

public record ErrorReportDto(
        String errorMessage,
        String payload
) {
}

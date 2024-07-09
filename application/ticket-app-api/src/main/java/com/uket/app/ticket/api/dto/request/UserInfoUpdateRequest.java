package com.uket.app.ticket.api.dto.request;

public record UserInfoUpdateRequest(
        String depositorName,
        String phoneNumber
) {
}

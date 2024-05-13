package com.uket.app.admin.api.dto.response;

public record EnterShowResponse(
        Long ticketId
) {

    public static EnterShowResponse of(Long ticketId) {
        return new EnterShowResponse(ticketId);
    }
}

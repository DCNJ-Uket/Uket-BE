package com.uket.app.ticket.api.dto.request;

public record UserRegisterRequest(
        String depositorName,
        String phoneNumber,
        String university,
        String studentMajor,
        String studentCode
) {

}

package com.uket.app.ticket.api.dto.request;

import jakarta.validation.constraints.Email;

public record AuthCodeRequest(
        @Email
        String email,
        Long universityId,
        String authCode
) {

}

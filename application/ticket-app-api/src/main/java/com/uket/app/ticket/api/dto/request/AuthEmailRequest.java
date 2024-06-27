package com.uket.app.ticket.api.dto.request;

import jakarta.validation.constraints.Email;

public record AuthEmailRequest(
        @Email
        String email,

        Long universityId
) {

}

package com.uket.app.ticket.api.dto.request;

import jakarta.validation.constraints.Email;

public record EmailRequest(
        @Email
        String email
) {

}

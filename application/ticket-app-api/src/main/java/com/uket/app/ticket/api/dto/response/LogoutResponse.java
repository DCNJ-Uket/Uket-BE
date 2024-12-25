package com.uket.app.ticket.api.dto.response;

import lombok.Builder;

@Builder
public record LogoutResponse(
    Boolean success
) {

}

package com.uket.app.ticket.api.dto.response;

import com.uket.domain.ticket.dto.TicketDto;

public record TicketingResponse(
    Boolean success,
    TicketDto ticket
) {

    public static TicketingResponse of(Boolean success, TicketDto ticket) {
        return new TicketingResponse(success, ticket);
    }
}

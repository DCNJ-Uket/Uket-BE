package com.uket.app.ticket.api.dto.request;

public record TicketingRequest(
    Long reservationId,
    Long universityId
) {

}

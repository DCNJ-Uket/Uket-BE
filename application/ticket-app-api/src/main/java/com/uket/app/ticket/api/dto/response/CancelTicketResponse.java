package com.uket.app.ticket.api.dto.response;


import com.uket.domain.ticket.dto.CancelTicketDto;

public record CancelTicketResponse(
    Long ticketId,
    String ticketStatus
) {
    public static CancelTicketResponse of(CancelTicketDto ticket) {
        return new CancelTicketResponse(ticket.ticketId(), ticket.ticketStatus());
    }
}

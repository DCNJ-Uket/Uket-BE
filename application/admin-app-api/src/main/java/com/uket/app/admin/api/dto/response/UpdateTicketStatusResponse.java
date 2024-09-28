package com.uket.app.admin.api.dto.response;

import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;

public record UpdateTicketStatusResponse(
        Long ticketId,
        TicketStatus status
) {

    public static UpdateTicketStatusResponse from(Ticket ticket) {
        return new UpdateTicketStatusResponse(ticket.getId(), ticket.getStatus());
    }
}

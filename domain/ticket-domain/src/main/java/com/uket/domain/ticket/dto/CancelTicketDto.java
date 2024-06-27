package com.uket.domain.ticket.dto;

import com.uket.domain.ticket.entity.Ticket;

public record CancelTicketDto(
    Long ticketId,

    String ticketStatus
) {
    public static CancelTicketDto from(Ticket ticket) {
        return new CancelTicketDto(ticket.getId(), ticket.getStatus().getValue());
    }
}

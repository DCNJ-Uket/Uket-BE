package com.uket.domain.ticket.dto;

import com.uket.domain.ticket.entity.Ticket;

public record TicketDto(
        Long userId,
        Long ticketId
) {

    public static TicketDto from(Ticket ticket) {
        return new TicketDto(ticket.getUser().getId(), ticket.getId());
    }
}

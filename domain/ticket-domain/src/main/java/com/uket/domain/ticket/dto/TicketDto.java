package com.uket.domain.ticket.dto;

import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import lombok.Builder;

@Builder
public record TicketDto(
        Long ticketId,
        Long userId,
        String userName,
        TicketStatus status
) {

    public static TicketDto from(Ticket ticket) {
        return TicketDto.builder()
                .ticketId(ticket.getId())
                .userId(ticket.getUser().getId())
                .userName(ticket.getUser().getName())
                .status(ticket.getStatus())
                .build();
    }
}

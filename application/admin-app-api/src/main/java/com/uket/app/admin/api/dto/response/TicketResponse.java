package com.uket.app.admin.api.dto.response;

import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import lombok.Builder;

import java.time.LocalDateTime;
import java.sql.Timestamp;

@Builder
public record TicketResponse(
    Long ticketId,
    String depositorName,
    LocalDateTime showDate,
    LocalDateTime showTime,
    Timestamp orderDate,
    Timestamp updatedDate,
    TicketStatus status,
    String userType
) {

    public static TicketResponse from(Ticket ticket) {
        return TicketResponse.builder()
            .ticketId(ticket.getId())
            .depositorName(ticket.getUser().getName())
            .showDate(ticket.getShow().getStartDate())
            .showTime(ticket.getReservation().getStartTime())
            .orderDate(ticket.getCreatedAt())
            .updatedDate(ticket.getModifiedAt())
            .status(ticket.getStatus())
            .userType(ticket.getReservation().getType().getValue())
            .build();
    }
}

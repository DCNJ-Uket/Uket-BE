package com.uket.app.admin.api.dto.response;

import com.uket.domain.ticket.dto.CheckTicketDto;
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
    String ticketStatus,
    String userType
) {

    public static TicketResponse from(CheckTicketDto checkTicketDto) {
        return TicketResponse.builder()
            .ticketId(checkTicketDto.ticketId())
            .depositorName(checkTicketDto.userName())
            .showDate(checkTicketDto.showDate())
            .showTime(checkTicketDto.enterStartTime())
            .orderDate(checkTicketDto.createdAt())
            .updatedDate(checkTicketDto.updatedAt())
            .ticketStatus(checkTicketDto.ticketStatus())
            .userType(checkTicketDto.userType())
            .build();
    }
}

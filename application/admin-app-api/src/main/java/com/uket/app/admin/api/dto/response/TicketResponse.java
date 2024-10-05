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
    LocalDateTime orderDate,
    LocalDateTime updatedDate,
    String ticketStatus,
    String userType
) {

    public static TicketResponse from(CheckTicketDto checkTicketDto) {
        return TicketResponse.builder()
            .ticketId(checkTicketDto.ticketId())
            .depositorName(checkTicketDto.userName())
            .showDate(checkTicketDto.showStartDate())
            .showTime(checkTicketDto.enterStartTime())
            .orderDate(checkTicketDto.createdAt())
            .updatedDate(checkTicketDto.updatedAt())
            .ticketStatus(checkTicketDto.ticketStatus())
            .userType(checkTicketDto.userType())
            .build();
    }
}

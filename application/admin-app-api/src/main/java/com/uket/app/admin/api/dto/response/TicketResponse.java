package com.uket.app.admin.api.dto.response;

import com.uket.app.admin.api.enums.MaskingType;
import com.uket.app.admin.api.aop.Mask;
import com.uket.domain.ticket.dto.CheckTicketDto;
import lombok.Builder;

import java.time.LocalDateTime;

@Builder
public record TicketResponse(
    Long ticketId,

    @Mask(type = MaskingType.NAME)
    String depositorName,
    @Mask(type = MaskingType.PHONE)
    String telephone,
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
            .telephone(checkTicketDto.phoneNumber())
            .showTime(checkTicketDto.showStartDate())
            .orderDate(checkTicketDto.createdAt())
            .updatedDate(checkTicketDto.updatedAt())
            .ticketStatus(checkTicketDto.ticketStatus())
            .userType(checkTicketDto.userType())
            .build();
    }
}

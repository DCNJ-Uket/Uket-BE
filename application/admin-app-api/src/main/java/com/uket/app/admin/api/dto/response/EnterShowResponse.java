package com.uket.app.admin.api.dto.response;

import com.uket.domain.ticket.dto.TicketDto;
import com.uket.domain.ticket.enums.TicketStatus;
import lombok.Builder;

@Builder
public record EnterShowResponse(
        Long ticketId,
        Long userId,
        String userName,
        TicketStatus status,
        String msg
) {

    public static EnterShowResponse of(TicketDto ticketDto) {
        return EnterShowResponse.builder()
                .ticketId(ticketDto.ticketId())
                .userId(ticketDto.userId())
                .userName(ticketDto.userName())
                .status(ticketDto.status())
                .msg(ticketDto.msg())
                .build();
    }
}

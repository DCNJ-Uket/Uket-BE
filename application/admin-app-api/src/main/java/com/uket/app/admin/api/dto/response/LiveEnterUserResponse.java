package com.uket.app.admin.api.dto.response;

import com.uket.app.admin.api.service.LiveEnterUserDto;
import com.uket.domain.ticket.enums.TicketStatus;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDateTime;

public record LiveEnterUserResponse(
        @Schema(description = "입장 시간")
        LocalDateTime enterTime,

        @Schema(description = "입금자명")
        String name,

        @Schema(description = "티켓 날짜")
        LocalDateTime ticketDate,

        @Schema(description = "전화번호")
        String phoneNumber,

        @Schema(description = "티켓 상태")
        TicketStatus ticketStatus
) {

    public static LiveEnterUserResponse from(LiveEnterUserDto liveEnterUserDto) {
        return new LiveEnterUserResponse(
                liveEnterUserDto.enterTime(),
                liveEnterUserDto.name(),
                liveEnterUserDto.ticketDate(),
                liveEnterUserDto.phoneNumber(),
                liveEnterUserDto.ticketStatus()
        );
    }
}

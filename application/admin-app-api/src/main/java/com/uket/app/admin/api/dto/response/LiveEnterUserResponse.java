package com.uket.app.admin.api.dto.response;

import com.uket.app.admin.api.aop.MaskingUtil;
import com.uket.app.admin.api.enums.MaskingType;
import com.uket.app.admin.api.aop.Mask;
import com.uket.app.admin.api.dto.LiveEnterUserDto;
import com.uket.domain.ticket.enums.TicketStatus;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDateTime;

public record LiveEnterUserResponse(
        @Schema(description = "입장 시간")
        LocalDateTime enterTime,

        @Schema(description = "입금자명")
        @Mask(type = MaskingType.NAME)
        String name,

        @Schema(description = "티켓 날짜")
        LocalDateTime ticketDate,

        @Schema(description = "전화번호")
        @Mask(type = MaskingType.PHONE)
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

    public LiveEnterUserResponse withMaskedValues() {
        String maskedName = MaskingUtil.MaskingOf(MaskingType.NAME, this.name);
        String maskedPhoneNumber = MaskingUtil.MaskingOf(MaskingType.PHONE, this.phoneNumber);

        return new LiveEnterUserResponse(
            this.enterTime,
            maskedName,
            this.ticketDate,
            maskedPhoneNumber,
            this.ticketStatus
        );
    }
}

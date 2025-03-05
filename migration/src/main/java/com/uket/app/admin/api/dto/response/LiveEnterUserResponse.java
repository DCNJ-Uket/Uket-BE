package com.uket.app.admin.api.dto.response;

import com.uket.app.admin.api.aop.MaskingUtil;
import com.uket.app.admin.api.enums.MaskingType;
import com.uket.app.admin.api.aop.Mask;
import com.uket.app.admin.api.dto.LiveEnterUserDto;
import com.uket.domain.ticket.enums.TicketStatus;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.ZoneId;
import java.time.ZonedDateTime;

public record LiveEnterUserResponse(
        @Schema(description = "입장 시간")
        ZonedDateTime enterTime,

        @Schema(description = "입금자명")
        String name,

        @Schema(description = "티켓 날짜")
        ZonedDateTime ticketDate,

        @Schema(description = "전화번호")
        @Mask(type = MaskingType.PHONE)
        String phoneNumber,

        @Schema(description = "티켓 상태")
        TicketStatus ticketStatus
) {
    private static final String zoneId = "Asia/Seoul";

    public static LiveEnterUserResponse from(LiveEnterUserDto liveEnterUserDto) {
        return new LiveEnterUserResponse(
                liveEnterUserDto.enterTime().atZone(ZoneId.of(zoneId)),
                liveEnterUserDto.name(),
                liveEnterUserDto.ticketDate().atZone(ZoneId.of(zoneId)),
                liveEnterUserDto.phoneNumber(),
                liveEnterUserDto.ticketStatus()
        );
    }

    public LiveEnterUserResponse withMaskedValues() {
        String maskedPhoneNumber = MaskingUtil.MaskingOf(MaskingType.PHONE, this.phoneNumber);

        return new LiveEnterUserResponse(
            this.enterTime,
            this.name,
            this.ticketDate,
            maskedPhoneNumber,
            this.ticketStatus
        );
    }
}

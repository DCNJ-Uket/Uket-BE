package com.uket.app.admin.api.dto.response;

import com.uket.app.admin.api.aop.MaskingUtil;
import com.uket.app.admin.api.enums.MaskingType;
import com.uket.app.admin.api.aop.Mask;
import com.uket.domain.ticket.dto.AdminCheckTicketDto;
import com.uket.domain.ticket.dto.CheckTicketDto;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import lombok.Builder;

@Builder
public record TicketResponse(
    Long ticketId,

    String depositorName,
    @Mask(type = MaskingType.PHONE)
    String telephone,
    ZonedDateTime showTime,
    ZonedDateTime orderDate,
    ZonedDateTime updatedDate,
    String ticketStatus,
    String userType
) {
    private static final String zoneId = "Asia/Seoul";

    public static TicketResponse from(AdminCheckTicketDto checkTicketDto) {
        return TicketResponse.builder()
            .ticketId(checkTicketDto.ticketId())
            .depositorName(checkTicketDto.userName())
            .telephone(checkTicketDto.phoneNumber())
            .showTime(checkTicketDto.showStartDate().atZone(ZoneId.of(zoneId)))
            .orderDate(checkTicketDto.createdAt().atZone(ZoneId.of(zoneId)))
            .updatedDate(checkTicketDto.updatedAt().atZone(ZoneId.of(zoneId)))
            .ticketStatus(checkTicketDto.ticketStatus())
            .userType(checkTicketDto.userType())
            .build();
    }

    public TicketResponse withMaskedValues() {
        String maskedTelephone = MaskingUtil.MaskingOf(MaskingType.PHONE, this.telephone);

        return new TicketResponse(
            this.ticketId,
            this.depositorName,
            maskedTelephone,
            this.showTime,
            this.orderDate,
            this.updatedDate,
            this.ticketStatus,
            this.userType
        );
    }
}

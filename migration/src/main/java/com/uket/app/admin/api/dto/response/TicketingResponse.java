package com.uket.app.admin.api.dto.response;

import com.uket.app.admin.api.aop.Mask;
import com.uket.app.admin.api.aop.MaskingUtil;
import com.uket.app.admin.api.dto.CheckTicketingDto;
import com.uket.app.admin.api.enums.MaskingType;
import com.uket.domain.form.dto.FormAnswerDto;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.List;
import lombok.Builder;

@Builder
public record TicketingResponse(
    Long ticketId,

    String depositorName,
    @Mask(type = MaskingType.PHONE)
    String telephone,
    ZonedDateTime showTime,
    ZonedDateTime orderDate,
    ZonedDateTime updatedDate,
    String ticketStatus,
    String userType,
    List<FormAnswerDto> formAnswers
) {
    private static final String zoneId = "Asia/Seoul";

    public static TicketingResponse from(CheckTicketingDto checkTicketingDto) {
        return TicketingResponse.builder()
            .ticketId(checkTicketingDto.ticket().ticketId())
            .depositorName(checkTicketingDto.ticket().userName())
            .telephone(checkTicketingDto.ticket().phoneNumber())
            .showTime(checkTicketingDto.ticket().showStartDate().atZone(ZoneId.of(zoneId)))
            .orderDate(checkTicketingDto.ticket().createdAt().atZone(ZoneId.of(zoneId)))
            .updatedDate(checkTicketingDto.ticket().updatedAt().atZone(ZoneId.of(zoneId)))
            .ticketStatus(checkTicketingDto.ticket().ticketStatus())
            .userType(checkTicketingDto.ticket().userType())
            .formAnswers(checkTicketingDto.formAnswers())
            .build();
    }

    public TicketingResponse withMaskedValues() {
        String maskedTelephone = MaskingUtil.MaskingOf(MaskingType.PHONE, this.telephone);

        return new TicketingResponse(
            this.ticketId,
            this.depositorName,
            maskedTelephone,
            this.showTime,
            this.orderDate,
            this.updatedDate,
            this.ticketStatus,
            this.userType,
            this.formAnswers
        );
    }
}

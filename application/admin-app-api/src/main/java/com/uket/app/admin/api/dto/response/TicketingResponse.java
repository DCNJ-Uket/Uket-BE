package com.uket.app.admin.api.dto.response;

import com.uket.app.admin.api.aop.Mask;
import com.uket.app.admin.api.aop.MaskingUtil;
import com.uket.app.admin.api.dto.CheckTicketingDto;
import com.uket.app.admin.api.enums.MaskingType;
import com.uket.domain.form.dto.AnswerDto;
import com.uket.domain.ticket.dto.CheckTicketDto;
import java.time.LocalDateTime;
import java.util.List;
import lombok.Builder;

@Builder
public record TicketingResponse(
    Long ticketId,

    @Mask(type = MaskingType.NAME)
    String depositorName,
    @Mask(type = MaskingType.PHONE)
    String telephone,
    LocalDateTime showTime,
    LocalDateTime orderDate,
    LocalDateTime updatedDate,
    String ticketStatus,
    String userType,
    List<AnswerDto> answers
) {

    public static TicketingResponse from(CheckTicketingDto checkTicketingDto) {
        return TicketingResponse.builder()
            .ticketId(checkTicketingDto.ticket().ticketId())
            .depositorName(checkTicketingDto.ticket().userName())
            .telephone(checkTicketingDto.ticket().phoneNumber())
            .showTime(checkTicketingDto.ticket().showStartDate())
            .orderDate(checkTicketingDto.ticket().createdAt())
            .updatedDate(checkTicketingDto.ticket().updatedAt())
            .ticketStatus(checkTicketingDto.ticket().ticketStatus())
            .userType(checkTicketingDto.ticket().userType())
            .answers(checkTicketingDto.answers())
            .build();
    }

    public TicketingResponse withMaskedValues() {
        String maskedDepositorName = MaskingUtil.MaskingOf(MaskingType.NAME, this.depositorName);
        String maskedTelephone = MaskingUtil.MaskingOf(MaskingType.PHONE, this.telephone);

        return new TicketingResponse(
            this.ticketId,
            maskedDepositorName,
            maskedTelephone,
            this.showTime,
            this.orderDate,
            this.updatedDate,
            this.ticketStatus,
            this.userType,
            this.answers
        );
    }
}

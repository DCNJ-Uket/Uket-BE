package com.uket.app.domain.user.admin.dto;

import com.uket.domain.form.dto.FormAnswerDto;
import com.uket.domain.ticket.dto.AdminCheckTicketDto;
import java.util.List;
import lombok.Builder;

@Builder
public record CheckTicketingDto(
        AdminCheckTicketDto ticket,
        List<FormAnswerDto> formAnswers
) {
    public static CheckTicketingDto of(AdminCheckTicketDto ticket, List<FormAnswerDto> answers) {
        return CheckTicketingDto.builder()
                .ticket(ticket)
                .formAnswers(answers)
                .build();
    }
}

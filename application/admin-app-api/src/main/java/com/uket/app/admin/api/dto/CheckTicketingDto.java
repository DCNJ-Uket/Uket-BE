package com.uket.app.admin.api.dto;

import com.uket.domain.form.dto.AnswerDto;
import com.uket.domain.form.dto.FormAnswerDto;
import com.uket.domain.ticket.dto.CheckTicketDto;
import java.util.List;
import lombok.Builder;

@Builder
public record CheckTicketingDto(
        CheckTicketDto ticket,
        List<FormAnswerDto> formAnswers
) {
    public static CheckTicketingDto of(CheckTicketDto ticket, List<FormAnswerDto> answers) {
        return CheckTicketingDto.builder()
                .ticket(ticket)
                .formAnswers(answers)
                .build();
    }
}

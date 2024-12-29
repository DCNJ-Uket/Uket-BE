package com.uket.app.admin.api.dto;

import com.uket.domain.form.dto.AnswerDto;
import com.uket.domain.ticket.dto.CheckTicketDto;
import java.util.List;
import lombok.Builder;

@Builder
public record CheckTicketingDto(
        CheckTicketDto ticket,
        List<AnswerDto> answers
) {
    public static CheckTicketingDto of(CheckTicketDto ticket, List<AnswerDto> answers) {
        return CheckTicketingDto.builder()
                .ticket(ticket)
                .answers(answers)
                .build();
    }
}

package com.uket.app.admin.api.service;

import com.uket.app.admin.api.dto.CheckTicketingDto;
import com.uket.domain.event.service.EventService;
import com.uket.domain.form.dto.AnswerDto;
import com.uket.domain.form.dto.FormAnswerDto;
import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.Survey;
import com.uket.domain.form.service.FormService;
import com.uket.domain.ticket.dto.CheckTicketDto;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class TicketSearchService {
    private final FormService formService;
    private final EventService eventService;

    public List<CheckTicketingDto> searchAllUserAnswersFromTickets(List<CheckTicketDto> tickets) {
        if (tickets.isEmpty()) {
            return List.of();
        }

        Long eventId = tickets.getFirst().eventId();
        Survey survey = eventService.findSurveyById(eventId);
        List<Form> forms = formService.findFormsBySurveyId(survey.getId());

        return tickets.stream()
                .map(ticket -> getCheckTicketingDto(ticket, forms)).toList();
    }

    private CheckTicketingDto getCheckTicketingDto(CheckTicketDto ticket, List<Form> forms) {
        Long userId = ticket.userId();
        List<FormAnswerDto> formAnswers = forms.stream()
                .map(form -> FormAnswerDto.from(
                        form,
                        formService.findAnswerByFormIdAndUserId(form.getId(), userId, form.getIsNecessary())
                ))
                .toList();
        return CheckTicketingDto.of(ticket, formAnswers);
    }
}

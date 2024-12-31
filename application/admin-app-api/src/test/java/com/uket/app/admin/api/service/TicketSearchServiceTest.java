package com.uket.app.admin.api.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import com.uket.app.admin.api.dto.CheckTicketingDto;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.service.EventService;
import com.uket.domain.form.dto.AnswerDto;
import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.FormType;
import com.uket.domain.form.entity.Options;
import com.uket.domain.form.entity.Survey;
import com.uket.domain.form.service.FormService;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.user.entity.Users;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class TicketSearchServiceTest {
    @InjectMocks
    private TicketSearchService ticketSearchService;

    @Mock
    private FormService formService;
    @Mock
    private EventService eventService;

    @Test
    void 티켓_예매목록에_질의응답_정보_추가() {
        //given
        List<CheckTicketDto> tickets = List.of(
                CheckTicketDto.builder().eventId(1L).userId(1L).build(),
                CheckTicketDto.builder().eventId(1L).userId(2L).build()
        );

        Events event = Events.builder().id(1L).build();
        Survey survey = Survey.builder().id(1L).build();

        Form form1 = Form.builder()
                .id(1L)
                .formType(FormType.TEXT)
                .maxLength(100)
                .question("sample text question")
                .build();

        Options option1 = Options.builder()
                .id(1L)
                .value("option1")
                .build();
        Options option2 = Options.builder()
                .id(1L)
                .value("option2")
                .build();
        Form form2 = Form.builder()
                .id(2L)
                .formType(FormType.DROPDOWN)
                .question("sample dropdown question")
                .options(List.of(option1, option2))
                .build();

        Users user1 = Users.builder()
                .id(1L)
                .build();
        Users user2 = Users.builder()
                .id(2L)
                .build();

        AnswerDto answerDto1 = AnswerDto.builder()
                .answerId(1L)
                .response("sample text answer")
                .build();
        AnswerDto answerDto2 = AnswerDto.builder()
                .answerId(2L)
                .response("option2")
                .build();
        AnswerDto answerDto3 = AnswerDto.builder()
                .answerId(1L)
                .response("sample text answer2")
                .build();
        AnswerDto answerDto4 = AnswerDto.builder()
                .answerId(2L)
                .response("option1")
                .build();

        when(eventService.findSurveyById(event.getId())).thenReturn(survey);
        when(formService.findFormsBySurveyId(survey.getId())).thenReturn(List.of(form1, form2));
        when(formService.findAnswerByFormIdAndUserId(form1.getId(), user1.getId())).thenReturn(answerDto1);
        when(formService.findAnswerByFormIdAndUserId(form2.getId(), user1.getId())).thenReturn(answerDto2);
        when(formService.findAnswerByFormIdAndUserId(form1.getId(), user2.getId())).thenReturn(answerDto3);
        when(formService.findAnswerByFormIdAndUserId(form2.getId(), user2.getId())).thenReturn(answerDto4);

        //when
        List<CheckTicketingDto> ticketingDtos = ticketSearchService.searchAllUserAnswersFromTickets(tickets);

        //then
        assertThat(ticketingDtos.size()).isEqualTo(2);
        assertThat(ticketingDtos.get(0).formAnswers().size()).isEqualTo(2);
        assertThat(ticketingDtos.get(0).formAnswers().get(0).answer()).isEqualTo("sample text answer");
        assertThat(ticketingDtos.get(1).formAnswers().get(1).answer()).isEqualTo("option1");
    }
}

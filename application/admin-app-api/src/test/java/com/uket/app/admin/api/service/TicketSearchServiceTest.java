package com.uket.app.admin.api.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.when;

import com.uket.app.admin.api.dto.CheckTicketingDto;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.service.EventService;
import com.uket.domain.form.dto.AnswerDto;
import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.FormType;
import com.uket.domain.form.entity.Options;
import com.uket.domain.form.entity.Survey;
import com.uket.domain.form.exception.FormException;
import com.uket.domain.form.service.FormService;
import com.uket.domain.ticket.dto.AdminCheckTicketDto;
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
        List<AdminCheckTicketDto> tickets = List.of(
                AdminCheckTicketDto.builder().eventId(1L).userId(1L).build(),
                AdminCheckTicketDto.builder().eventId(1L).userId(2L).build()
        );

        Events event = Events.builder().id(1L).build();
        Survey survey = Survey.builder().id(1L).build();

        Form form1 = Form.builder()
                .id(1L)
                .formType(FormType.TEXT)
                .maxLength(100)
                .question("sample text question")
                .isNecessary(true)
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
                .isNecessary(true)
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
        when(formService.findAnswerByFormIdAndUserId(form1.getId(), user1.getId(), form1.getIsNecessary())).thenReturn(answerDto1);
        when(formService.findAnswerByFormIdAndUserId(form2.getId(), user1.getId(), form2.getIsNecessary())).thenReturn(answerDto2);
        when(formService.findAnswerByFormIdAndUserId(form1.getId(), user2.getId(), form1.getIsNecessary())).thenReturn(answerDto3);
        when(formService.findAnswerByFormIdAndUserId(form2.getId(), user2.getId(), form2.getIsNecessary())).thenReturn(answerDto4);

        //when
        List<CheckTicketingDto> ticketingDtos = ticketSearchService.searchAllUserAnswersFromTickets(tickets);

        //then
        assertThat(ticketingDtos.size()).isEqualTo(2);
        assertThat(ticketingDtos.get(0).formAnswers().size()).isEqualTo(2);
        assertThat(ticketingDtos.get(0).formAnswers().get(0).answer()).isEqualTo("sample text answer");
        assertThat(ticketingDtos.get(1).formAnswers().get(1).answer()).isEqualTo("option1");
    }

    @Test
    void 필수응답_예외처리() {
        //given
        List<AdminCheckTicketDto> tickets1 = List.of(
                AdminCheckTicketDto.builder().eventId(1L).userId(1L).build()
        );
        List<AdminCheckTicketDto> tickets2 = List.of(
                AdminCheckTicketDto.builder().eventId(2L).userId(2L).build()
        );

        Events event = Events.builder().id(1L).build();
        Survey survey = Survey.builder().id(1L).build();
        Events event2 = Events.builder().id(2L).build();
        Survey survey2 = Survey.builder().id(2L).build();

        Form form1 = Form.builder()
                .id(1L)
                .formType(FormType.TEXT)
                .maxLength(100)
                .question("sample text question")
                .isNecessary(true)
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
                .isNecessary(true)
                .build();

        Users user1 = Users.builder()
                .id(1L)
                .build();
        Users user2 = Users.builder()
                .id(2L)
                .build();

        when(eventService.findSurveyById(event.getId())).thenReturn(survey);
        when(eventService.findSurveyById(event2.getId())).thenReturn(survey2);
        when(formService.findFormsBySurveyId(survey.getId())).thenReturn(List.of(form1));
        when(formService.findFormsBySurveyId(survey2.getId())).thenReturn(List.of(form2));
        when(formService.findAnswerByFormIdAndUserId(form1.getId(), user1.getId(), form1.getIsNecessary())).thenThrow(new FormException(ErrorCode.NOT_FOUND_RESPONSE));
        when(formService.findAnswerByFormIdAndUserId(form2.getId(), user2.getId(), form2.getIsNecessary())).thenThrow(new FormException(ErrorCode.NOT_FOUND_NECESSARY_RESPONSE));

        //when
        assertThatThrownBy(() -> ticketSearchService.searchAllUserAnswersFromTickets(tickets1))
                .isInstanceOf(FormException.class)
                .hasMessage(ErrorCode.NOT_FOUND_RESPONSE.getMessage());
        assertThatThrownBy(() -> ticketSearchService.searchAllUserAnswersFromTickets(tickets2))
                .isInstanceOf(FormException.class)
                .hasMessage(ErrorCode.NOT_FOUND_NECESSARY_RESPONSE.getMessage());

        //then
    }
}

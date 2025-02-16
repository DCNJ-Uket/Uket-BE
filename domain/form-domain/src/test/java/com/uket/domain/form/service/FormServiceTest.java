package com.uket.domain.form.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.uket.domain.form.dto.FormResponseDto;
import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.FormType;
import com.uket.domain.form.entity.Survey;
import com.uket.domain.form.exception.FormException;
import com.uket.domain.form.repository.AnswerRepository;
import com.uket.domain.form.repository.FormRepository;
import com.uket.domain.form.repository.SurveyRepository;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.repository.UserRepository;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class FormServiceTest {

    @InjectMocks
    private FormService formService;

    @Mock
    private SurveyRepository surveyRepository;

    @Mock
    private AnswerRepository answerRepository;

    @Mock
    private UserRepository userRepository;

    @Mock
    private FormRepository formRepository;

    @Test
    void Survey를_찾아_반환한다() {
        //given
        Survey survey = Survey.builder()
                .id(1L)
                .build();

        //when
        when(surveyRepository.findById(1L)).thenReturn(Optional.of(survey));

        //then
        Survey foundSurvey = formService.findById(survey.getId());
        assertThat(foundSurvey.getId()).isEqualTo(survey.getId());
    }

    @Test
    void Survey가_없는_경우_예외를_던진다() {
        //given
        Survey survey = Survey.builder()
                .id(1L)
                .build();

        //when
        when(surveyRepository.findById(1L)).thenReturn(Optional.empty());

        //then
        assertThrows(FormException.class, () -> formService.findById(survey.getId()));
    }

    @Test
    void 응답을_제출한다() {
        //given
        Survey survey = Survey.builder()
                .id(1L)
                .forms(new ArrayList<>())
                .build();
        Form form1 = Form.builder()
                .id(1L)
                .survey(survey)
                .formType(FormType.TEXT)
                .question("사연은?")
                .maxLength(500)
                .build();
        survey.getForms().add(form1);
        Form form2 = Form.builder()
                .id(2L)
                .survey(survey)
                .formType(FormType.TEXT)
                .question("이름은?")
                .maxLength(500)
                .build();
        survey.getForms().add(form2);

        Users user = Users.builder()
                .id(1L)
                .name("홍길동")
                .build();

        List<FormResponseDto> responses = List.of(
                new FormResponseDto(1L, "딱히 없음"),
                new FormResponseDto(2L, "이00")
        );

        //when
        when(surveyRepository.findById(1L)).thenReturn(Optional.of(survey));
        when(userRepository.findById(1L)).thenReturn(Optional.of(user));
        when(answerRepository.saveAll(any())).thenReturn(null);
        when(formRepository.findById(1L)).thenReturn(Optional.of(form1));
        when(formRepository.findById(2L)).thenReturn(Optional.of(form2));

        //then
        assertDoesNotThrow(() -> formService.submitResponse(survey.getId(), user.getId(), responses));
    }
}

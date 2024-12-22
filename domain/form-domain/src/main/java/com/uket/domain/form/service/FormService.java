package com.uket.domain.form.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.dto.FormResponseDto;
import com.uket.domain.form.entity.Answer;
import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.Survey;
import com.uket.domain.form.exception.FormException;
import com.uket.domain.form.repository.AnswerRepository;
import com.uket.domain.form.repository.SurveyRepository;
import com.uket.domain.user.dto.UserDto;
import com.uket.domain.user.entity.Users;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.security.SecurityProperties.User;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class FormService {
    private final SurveyRepository surveyRepository;
    private final AnswerRepository answerRepository;

    public Survey findById(long surveyId) {
        return surveyRepository.findById(surveyId)
                .orElseThrow(() -> new FormException(ErrorCode.UNKNOWN_SERVER_ERROR));
    }

    public void submitResponse(long surveyId, Users user, List<FormResponseDto> responses) {
        Survey survey = surveyRepository.findById(surveyId)
                .orElseThrow(() -> new FormException(ErrorCode.UNKNOWN_SERVER_ERROR));

        List<Answer> answers = createAnswers(survey.getForms(), user, responses);
        answers.forEach(Answer::validate);
        answerRepository.saveAll(answers);
    }

    private List<Answer> createAnswers(List<Form> forms, Users user, List<FormResponseDto> responses) {
        List<Answer> answers = new ArrayList<>();
        Map<Long, Form> formMap = forms.stream().collect(Collectors.toMap(Form::getId, form -> form));
        for(FormResponseDto res : responses) {
            Form f = formMap.get(res.formId());
            if(f == null)
                throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
            answers.add(new Answer(f, user, res.response()));
        }
        return answers;
    }
}

package com.uket.domain.form.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.entity.Answer;
import com.uket.domain.form.entity.TextAnswer;
import com.uket.domain.form.entity.Survey;
import com.uket.domain.form.exception.FormException;
import com.uket.domain.form.repository.AnswerRepository;
import com.uket.domain.form.repository.SurveyRepository;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class FormService {
    private final SurveyRepository surveyRepository;
    private final AnswerRepository answerRepository;

    public Survey findById(int surveyId) {
        return surveyRepository.findById(surveyId)
                .orElseThrow(() -> new FormException(ErrorCode.UNKNOWN_SERVER_ERROR));
    }

    public void submitResponse(int surveyId, List<Answer> answers) {
        Survey survey = surveyRepository.findById(surveyId)
                .orElseThrow(() -> new FormException(ErrorCode.UNKNOWN_SERVER_ERROR));
        survey.validateAnswers(answers);
//        answers.forEach(answerRepository::save);
    }
}

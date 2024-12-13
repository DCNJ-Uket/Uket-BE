package com.uket.domain.form.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.entity.Answer;
import com.uket.domain.form.entity.Survey;
import com.uket.domain.form.exception.FormException;
import com.uket.domain.form.repository.AnswerRepository;
import com.uket.domain.form.repository.SurveyRepository;
import java.util.List;
import java.util.Map;
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

    public void submitResponse(int surveyId, Map<Long, String> responses) {
        Survey survey = surveyRepository.findById(surveyId)
                .orElseThrow(() -> new FormException(ErrorCode.UNKNOWN_SERVER_ERROR));

        List<Answer> answers = survey.submitAnswers(responses);

        answers.forEach(answerRepository::save);
    }
}

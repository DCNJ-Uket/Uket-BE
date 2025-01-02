package com.uket.app.ticket.api.dto.response;

import com.uket.domain.form.entity.Answer;
import java.util.List;
import lombok.Builder;

@Builder
public record SurveyAnswerResponse(
    Long surveyId,

    Long userId,

    List<String> userAnswers
) {
    public static SurveyAnswerResponse from(Long surveyId, Long userId, List<Answer> answers) {
        return SurveyAnswerResponse.builder()
            .surveyId(surveyId)
            .userId(userId)
            .userAnswers(answers.stream().map(Answer::getResponse).toList())
            .build();
    }
}

package com.uket.app.ticket.api.dto.response;

import com.uket.domain.form.dto.FormDto;
import com.uket.domain.form.entity.Survey;
import java.util.List;
import lombok.Builder;

@Builder
public record SurveyResponse(
    Long surveyId,
    List<FormDto> forms

) {
    public static SurveyResponse from(Survey survey, List<FormDto> forms) {
        return SurveyResponse.builder()
            .surveyId(survey.getId())
            .forms(forms)
            .build();
    }
}

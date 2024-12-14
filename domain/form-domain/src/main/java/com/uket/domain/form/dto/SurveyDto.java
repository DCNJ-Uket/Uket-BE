package com.uket.domain.form.dto;

import com.uket.domain.form.entity.Survey;
import java.util.List;
import lombok.Builder;

@Builder
public record SurveyDto(
        Long id,
        List<FormDto> forms
) {
    public static SurveyDto from(Survey survey) {
        return SurveyDto.builder()
                .id(survey.getId())
                .forms(survey.getTextForms().stream().map(FormDto::from).toList())
                .build();
    }
}

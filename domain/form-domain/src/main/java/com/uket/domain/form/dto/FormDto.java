package com.uket.domain.form.dto;

import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.FormType;
import java.util.List;
import lombok.Builder;

@Builder
public record FormDto(
        Long id,
        Long surveyId,
        FormType formType,
        List<OptionDto> options,
        String question,
        Integer maxLength
) {
    public static FormDto from(Form form) {
        return FormDto.builder()
                .id(form.getId())
                .surveyId(form.getSurvey().getId())
                .formType(form.getFormType())
                .options(form.getOptions().stream().map(OptionDto::from).toList())
                .question(form.getQuestion())
                .maxLength(form.getMaxLength())
                .build();
    }
}

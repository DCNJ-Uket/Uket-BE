package com.uket.domain.form.dto;

import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.FormType;
import com.uket.domain.form.entity.Option;
import com.uket.domain.form.entity.Survey;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import java.util.List;
import lombok.Builder;

@Builder
public record FormDto(
        Long id,
        Long surveyId,
        FormType formType,
        List<OptionDto> options,
        String question
) {
    public static FormDto from(Form form) {
        return FormDto.builder()
                .id(form.getId())
                .surveyId(form.getSurvey().getId())
                .formType(form.getFormType())
                .options(form.getOptions().stream().map(OptionDto::from).toList())
                .question(form.getQuestion())
                .build();
    }
}

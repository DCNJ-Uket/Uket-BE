package com.uket.domain.form.dto;

import com.uket.domain.form.entity.FormType;
import com.uket.domain.form.entity.TextForm;
import lombok.Builder;

@Builder
public record TextFormDto(
         Long id,
         FormType formType,
         String question
) implements FormDto {
    public static TextFormDto from(TextForm form) {
        return TextFormDto.builder()
                .id(form.getId())
                .formType(form.getFormType())
                .question(form.getQuestion())
                .build();
    }
}

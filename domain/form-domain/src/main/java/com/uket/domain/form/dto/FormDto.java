package com.uket.domain.form.dto;

import com.uket.domain.form.entity.TextForm;
import lombok.Builder;

@Builder
public record FormDto(
         Long id,
         String question
) {
    public static FormDto from(TextForm textForm) {
        return FormDto.builder()
                .id(textForm.getId())
                .question(textForm.getQuestion())
                .build();
    }
}

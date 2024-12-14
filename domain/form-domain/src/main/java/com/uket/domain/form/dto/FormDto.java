package com.uket.domain.form.dto;

import com.uket.domain.form.entity.Form;
import lombok.Builder;

@Builder
public record FormDto(
         Long id,
         String question
) {
    public static FormDto from(Form form) {
        return FormDto.builder()
                .id(form.getId())
                .question(form.getQuestion())
                .build();
    }
}

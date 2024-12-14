package com.uket.domain.form.dto;

import com.uket.domain.form.entity.TextForm;
import lombok.Builder;

@Builder
public record TextFormDto(
         Long id,
         String question
) implements FormDto {
    public static TextFormDto from(TextForm textForm) {
        return TextFormDto.builder()
                .id(textForm.getId())
                .question(textForm.getQuestion())
                .build();
    }
}

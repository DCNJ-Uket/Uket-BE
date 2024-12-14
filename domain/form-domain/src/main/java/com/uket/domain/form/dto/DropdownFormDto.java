package com.uket.domain.form.dto;

import com.uket.domain.form.entity.DropdownForm;
import java.util.List;
import lombok.Builder;

@Builder
public record DropdownFormDto(
         Long id,
         String question,
         List<String> items
) {
    public static DropdownFormDto from(DropdownForm form) {
        return DropdownFormDto.builder()
                .id(form.getId())
                .question(form.getQuestion())
                .items(form.getItems())
                .build();
    }
}

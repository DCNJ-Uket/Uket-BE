package com.uket.domain.form.dto;

import com.uket.domain.form.entity.DropdownForm;
import com.uket.domain.form.entity.FormType;
import java.util.List;
import lombok.Builder;

@Builder
public record DropdownFormDto(
         Long id,
         FormType formType,
         String question,
         List<String> items
) implements FormDto {
    public static DropdownFormDto from(DropdownForm form) {
        return DropdownFormDto.builder()
                .id(form.getId())
                .formType(form.getFormType())
                .question(form.getQuestion())
                .items(form.getItems())
                .build();
    }
}

package com.uket.domain.form.dto;

import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.FormType;
import java.util.List;
import lombok.Builder;

@Builder
public record FormDto(
    Long formId,
    Boolean isNecessary,
    FormType formType,
    List<OptionDto> options,
    String question,
    Integer maxLength
) {
    public static FormDto from(Form form, List<OptionDto> options) {
        return FormDto.builder()
            .formId(form.getId())
            .isNecessary(form.getIsNecessary())
            .formType(form.getFormType())
            .options(options)
            .question(form.getQuestion())
            .maxLength(form.getMaxLength())
            .build();
    }
}

package com.uket.domain.form.dto;

import com.uket.domain.form.entity.Options;
import lombok.Builder;

@Builder
public record OptionDto(
        Long id,
        Long formId,
        String value
) {
    public static OptionDto from(Options option) {
        return OptionDto.builder()
                .id(option.getId())
                .formId(option.getForm().getId())
                .value(option.getValue())
                .build();
    }
}

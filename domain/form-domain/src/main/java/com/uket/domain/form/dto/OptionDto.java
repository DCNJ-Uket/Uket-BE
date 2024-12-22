package com.uket.domain.form.dto;

import com.uket.domain.form.entity.Option;
import lombok.Builder;

@Builder
public record OptionDto(
        Long id,
        Long formId,
        String value
) {
    public static OptionDto from(Option option) {
        return OptionDto.builder()
                .id(option.getId())
                .formId(option.getForm().getId())
                .value(option.getValue())
                .build();
    }
}

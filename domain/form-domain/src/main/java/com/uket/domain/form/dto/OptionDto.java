package com.uket.domain.form.dto;

import com.uket.domain.form.entity.Options;
import lombok.Builder;

@Builder
public record OptionDto(
        Long optionId,
        String value
) {
    public static OptionDto from(Options option) {
        return OptionDto.builder()
                .optionId(option.getId())
                .value(option.getValue())
                .build();
    }
}

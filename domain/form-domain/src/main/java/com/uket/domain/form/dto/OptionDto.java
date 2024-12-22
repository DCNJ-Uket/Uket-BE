package com.uket.domain.form.dto;

import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.Option;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
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

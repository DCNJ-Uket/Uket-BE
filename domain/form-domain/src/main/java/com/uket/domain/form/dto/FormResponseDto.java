package com.uket.domain.form.dto;

import lombok.Builder;

@Builder
public record FormResponseDto(
        Long formId,
        String response
) {

}

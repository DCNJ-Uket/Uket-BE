package com.uket.domain.form.dto;

import lombok.Builder;

@Builder
public record UserResponseDto(
        Long formId,
        String response
) {

}

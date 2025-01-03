package com.uket.domain.form.dto;

import com.uket.domain.form.entity.Answer;
import com.uket.domain.user.entity.Users;
import lombok.Builder;

@Builder
public record AnswerDto(
        Long answerId,
        Long formId,
        Long userId,
        String response
) {
    public static AnswerDto from(Answer answer) {
        return AnswerDto.builder()
                .answerId(answer.getId())
                .formId(answer.getForm().getId())
                .userId(answer.getUser().getId())
                .build();
    }

    public static AnswerDto noAnswerDto = new AnswerDto(-1L, -1L, -1L, "");
}

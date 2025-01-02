package com.uket.domain.form.dto;

import com.uket.domain.form.entity.Form;
import lombok.Builder;

@Builder
public record FormAnswerDto(
        Long formId,
        Long answerId,
        String question,
        String answer
) {
    public static FormAnswerDto from(Form form, AnswerDto answerDto) {
        return FormAnswerDto.builder()
                .formId(form.getId())
                .answerId(answerDto.answerId())
                .question("지인") // TODO 소리터 버전 한정
                .answer(answerDto.response())
                .build();
    }
}

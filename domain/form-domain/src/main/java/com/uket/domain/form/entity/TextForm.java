package com.uket.domain.form.entity;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.exception.FormException;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class TextForm {
    private Long id;
    private String question;

    public void validateAnswer(TextAnswer textAnswer) {
        if(textAnswer.getContent().isEmpty())
            throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
    }
}
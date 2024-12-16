package com.uket.domain.form.entity;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.exception.FormException;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
public class TextAnswer extends Answer {

    public TextAnswer(Long id, Long formId, String question, String response) {
        super(id, formId, question, response);
    }

    @Override
    public void validate() {
        if(getResponse().isEmpty())
            throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
    }
}

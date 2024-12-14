package com.uket.domain.form.entity;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.exception.FormException;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.w3c.dom.Text;

@Getter
@AllArgsConstructor
public class TextForm implements Form {
    private Long id;
    private String question;

    @Override
    public void validateAnswer(Answer answer) {
        if(!(answer instanceof TextAnswer))
            throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
        TextAnswer textAnswer = (TextAnswer) answer;

        if(textAnswer.getContent().isEmpty())
            throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
    }
}
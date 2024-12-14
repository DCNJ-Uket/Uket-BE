package com.uket.domain.form.entity;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.exception.FormException;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class Survey {
    private Long id;
    private List<TextForm> textForms;

    public void validateAnswers(List<TextAnswer> textAnswers) {
        Map<Long, TextAnswer> answerMap = textAnswers.stream()
                .collect(Collectors.toMap(TextAnswer::getFormId, textAnswer -> textAnswer));
        for(TextForm textForm : textForms) {
            TextAnswer textAnswer = answerMap.get(textForm.getId());
            if(textAnswer == null)
                throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
            textForm.validateAnswer(textAnswer);
        }
    }
}

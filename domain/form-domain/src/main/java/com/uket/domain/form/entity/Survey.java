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
    private List<Form> forms;

    public void validateAnswers(List<Answer> answers) {
        Map<Long, Answer> answerMap = answers.stream()
                .collect(Collectors.toMap(Answer::getFormId, answer -> answer));
        for(Form form : forms) {
            Answer answer = answerMap.get(form.getId());
            if(answer == null)
                throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
            form.validateAnswer(answer);
        }
    }
}

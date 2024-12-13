package com.uket.domain.form.entity;

import com.uket.domain.form.exception.FormException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class Survey {
    private Long id;
    private List<Form> forms;

    public List<Answer> submitAnswers(Map<Long, String> responses) {
        List<Answer> answers = new ArrayList<>();

        for(Form form : forms) {
            String response = responses.get(form.getId());
            if(response == null)
                throw new FormException("");

            answers.add(form.submitAnswer(response));
        }

        return answers;
    }
}

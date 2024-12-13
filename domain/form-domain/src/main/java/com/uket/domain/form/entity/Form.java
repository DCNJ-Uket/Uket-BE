package com.uket.domain.form.entity;

import com.uket.domain.form.exception.FormException;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class Form {
    private Long id;
    private String question;
    private String answer;

    public Answer submitAnswer(String response) {
        if(response.isEmpty())
            throw new FormException("");
        return new Answer(-1L, id, response);
    }
}
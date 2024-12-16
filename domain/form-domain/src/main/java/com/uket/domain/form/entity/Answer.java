package com.uket.domain.form.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public abstract class Answer {
    private Long id;
    private Long formId;
    private String question;
    private String response;

    abstract public void validate();
}

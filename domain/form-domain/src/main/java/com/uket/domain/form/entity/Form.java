package com.uket.domain.form.entity;

import lombok.Getter;

@Getter
public abstract class Form {
    private Long id;
    private FormType formType;
    private String question;
}

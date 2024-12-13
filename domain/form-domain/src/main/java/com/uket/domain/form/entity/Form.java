package com.uket.domain.form.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class Form {
    private Long id;
    private String question;
    private String answer;
}
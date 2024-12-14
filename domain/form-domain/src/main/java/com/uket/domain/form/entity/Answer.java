package com.uket.domain.form.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class Answer {
    private Long id;
    private Long formId;
    private String content;
}

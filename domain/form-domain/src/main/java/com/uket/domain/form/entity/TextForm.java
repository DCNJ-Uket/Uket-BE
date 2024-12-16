package com.uket.domain.form.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class TextForm extends Form {
    @Override
    Answer createAnswer(String response) {
        return new TextAnswer(this.getId(), this.getQuestion(), response);
    }
}
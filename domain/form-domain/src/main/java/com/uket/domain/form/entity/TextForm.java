package com.uket.domain.form.entity;

import com.uket.domain.user.entity.Users;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class TextForm extends Form {
    @Override
    Answer createAnswer(Users user, String response) {
        return new TextAnswer(this, user, this.getQuestion(), response);
    }
}
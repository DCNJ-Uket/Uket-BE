package com.uket.domain.form.entity;

import com.uket.domain.user.entity.Users;
import lombok.Getter;

@Getter
public abstract class Form {
    private Long id;
    private FormType formType;
    private String question;

    abstract Answer createAnswer(Users user, String response);
}

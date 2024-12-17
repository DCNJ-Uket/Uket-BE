package com.uket.domain.form.entity;

import com.uket.domain.user.entity.Users;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public class DropdownForm extends Form {
    private List<String> items;

    @Override
    Answer createAnswer(Users user, String response) {
        return new DropdownAnswer(this, user, this.getQuestion(), response, items.size());
    }
}

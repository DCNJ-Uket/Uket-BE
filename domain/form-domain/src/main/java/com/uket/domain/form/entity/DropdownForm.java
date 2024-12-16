package com.uket.domain.form.entity;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.exception.FormException;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public class DropdownForm extends Form {
    private List<String> items;

    @Override
    Answer createAnswer(String response) {
        return new DropdownAnswer(this.getId(), this.getQuestion(), response, items.size());
    }
}

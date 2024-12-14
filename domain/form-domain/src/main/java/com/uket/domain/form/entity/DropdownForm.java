package com.uket.domain.form.entity;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.exception.FormException;
import java.util.List;
import lombok.Getter;

@Getter
public class DropdownForm implements Form {
    private Long id;
    private String question;
    private List<String> items;

    @Override
    public void validateAnswer(Answer answer) {
        DropdownAnswer dropdownAnswer = (DropdownAnswer) answer;
        if(0 < dropdownAnswer.getSelectedItem() && dropdownAnswer.getSelectedItem() < items.size())
            throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
    }
}

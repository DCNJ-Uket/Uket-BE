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

    public void validateAnswer(DropdownAnswer answer) {
        if(0 < answer.getSelectedItem() && answer.getSelectedItem() < items.size())
            throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
    }
}

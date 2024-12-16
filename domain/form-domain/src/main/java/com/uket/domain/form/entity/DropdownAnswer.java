package com.uket.domain.form.entity;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.exception.FormException;
import lombok.Getter;

@Getter
public class DropdownAnswer extends Answer {
    private Integer itemSize;

    public DropdownAnswer(Long formId, String question, String response, Integer items) {
        super(formId, question, response);
        this.itemSize = items;
    }

    @Override
    public void validate() {
        int index = Integer.parseInt(getResponse());
        if(itemSize <= index || index < 0)
            throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
    }
}

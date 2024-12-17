package com.uket.domain.form.entity;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.exception.FormException;
import com.uket.domain.user.entity.Users;
import lombok.Getter;

@Getter
public class DropdownAnswer extends Answer {
    private Integer itemSize;

    public DropdownAnswer(Form form, Users user, String question, String response, Integer items) {
        super(form, user, question, response);
        this.itemSize = items;
    }

    @Override
    public void validate() {
        int index = Integer.parseInt(getResponse());
        if(itemSize <= index || index < 0)
            throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
    }
}

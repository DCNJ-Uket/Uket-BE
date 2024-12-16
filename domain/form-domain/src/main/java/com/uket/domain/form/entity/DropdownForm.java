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
        return new DropdownAnswer(-1L, this.getId(), this.getQuestion(), response, items.size());
    }

//    @Override
//    public void validateAnswer(Answer answer) {
//        if(!(answer instanceof DropdownAnswer))
//            throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
//        DropdownAnswer dropdownAnswer = (DropdownAnswer) answer;
//
//        if(0 < dropdownAnswer.getSelectedItem() && dropdownAnswer.getSelectedItem() < items.size())
//            throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
//    }
}

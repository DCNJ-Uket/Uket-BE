package com.uket.domain.form.entity;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.exception.FormException;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.w3c.dom.Text;

@Getter
@AllArgsConstructor
public class TextForm extends Form {
    @Override
    Answer createAnswer(String response) {
        return new TextAnswer(this.getId(), this.getQuestion(), response);
    }
}
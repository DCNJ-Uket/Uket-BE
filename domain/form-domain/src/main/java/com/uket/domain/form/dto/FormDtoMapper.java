package com.uket.domain.form.dto;

import com.uket.domain.form.entity.DropdownForm;
import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.TextForm;

public class FormDtoMapper {
    public static FormDto toDto(Form form) {
        if(form instanceof TextForm)
            return TextFormDto.from((TextForm) form);
        if(form instanceof DropdownForm)
            return DropdownFormDto.from((DropdownForm) form);
        return null;
    }
}

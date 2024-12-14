package com.uket.domain.form.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class DropdownAnswer {
    private Long id;
    private Long formId;
    private Integer selectedItem;
}

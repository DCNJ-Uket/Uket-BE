package com.uket.domain.form.entity;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class Survey {
    private Long id;
    List<Form> forms;
}

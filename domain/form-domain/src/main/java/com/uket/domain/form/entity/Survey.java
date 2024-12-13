package com.uket.domain.form.entity;

import java.util.List;
import lombok.AllArgsConstructor;

@AllArgsConstructor
public class Survey {
    private Long id;
    List<Form> forms;
}

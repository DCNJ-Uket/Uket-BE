package com.uket.domain.form.repository;

import com.uket.domain.form.entity.Form;
import java.util.Optional;

public interface FormRepository {
    Optional<Form> findById(int id);
}

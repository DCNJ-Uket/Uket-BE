package com.uket.domain.form.repository;

import com.uket.domain.form.entity.TextForm;
import java.util.Optional;

public interface FormRepository {
    Optional<TextForm> findById(int id);
}

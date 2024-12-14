package com.uket.domain.form.repository;

import com.uket.domain.form.entity.TextAnswer;
import java.util.Optional;

public interface AnswerRepository {
    Optional<TextAnswer> findById(int id);
    void save(TextAnswer textAnswer);
}

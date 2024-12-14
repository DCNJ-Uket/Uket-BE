package com.uket.domain.form.repository;

import com.uket.domain.form.entity.Answer;
import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.TextAnswer;
import java.util.Optional;

public interface AnswerRepository {
    Optional<Form> findById(int id);
    void save(Answer answer);
}

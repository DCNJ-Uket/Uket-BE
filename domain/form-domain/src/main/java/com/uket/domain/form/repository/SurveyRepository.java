package com.uket.domain.form.repository;

import com.uket.domain.form.entity.Survey;
import java.util.Optional;

public interface SurveyRepository {
    Optional<Survey> findById(int id);
}

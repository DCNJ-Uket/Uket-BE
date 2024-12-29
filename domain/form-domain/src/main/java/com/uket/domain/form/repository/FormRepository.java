package com.uket.domain.form.repository;

import com.uket.domain.form.entity.Form;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface FormRepository extends JpaRepository<Form, Long> {
    List<Form> findBySurveyId(Long surveyId);
}

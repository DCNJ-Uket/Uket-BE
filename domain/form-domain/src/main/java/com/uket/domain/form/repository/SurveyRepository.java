package com.uket.domain.form.repository;

import com.uket.domain.form.entity.Survey;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;

public interface SurveyRepository extends JpaRepository<Survey, Long> {
}

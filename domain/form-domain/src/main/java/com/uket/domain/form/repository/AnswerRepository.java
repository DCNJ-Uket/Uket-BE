package com.uket.domain.form.repository;

import com.uket.domain.form.entity.Answer;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AnswerRepository extends JpaRepository<Answer, Long> {
    Answer findAnswerByFormIdAndUserId(Long formId, Long userId);


    @Query(value = "SELECT * FROM answer a " +
        "WHERE a.form_id = :formId AND a.user_id = :userId " +
        "ORDER BY a.created_at DESC LIMIT 1", nativeQuery = true)
    Answer findRecentAnswerByFormIdAndUserId(Long formId, Long userId);
    void deleteAllByUserId(Long userId);
}

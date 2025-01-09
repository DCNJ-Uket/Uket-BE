package com.uket.domain.form.repository;

import com.uket.domain.form.entity.Answer;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AnswerRepository extends JpaRepository<Answer, Long> {
    Answer findAnswerByFormIdAndUserId(Long formId, Long userId);


    @Query("SELECT a FROM Answer a " +
        "WHERE a.form.id = :formId AND a.user.id = :userId " +
        "ORDER BY a.createdAt DESC")
    Answer findRecentAnswerByFormIdAndUserId(Long formId, Long userId);
    void deleteAllByUserId(Long userId);
}

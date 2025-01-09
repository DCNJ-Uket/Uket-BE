package com.uket.domain.form.repository;

import com.uket.domain.form.entity.Answer;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AnswerRepository extends JpaRepository<Answer, Long> {
    void deleteAllByUserId(Long userId);

    @Query("DELETE FROM Answer a WHERE a.form.id IN (" +
        "  SELECT f.id FROM Form f WHERE f.survey.id = (" +
        "    SELECT e.survey.id FROM Events e WHERE e.id = :eventId" +
        "  )" +
        ") AND a.user.id = :userId")
    void deleteAllByUserIdAndEventId(Long userId, Long eventId);
}

package com.uket.domain.form.repository;

import com.uket.domain.form.entity.Answer;
import com.uket.domain.form.entity.Form;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface AnswerRepository extends JpaRepository<Answer, Long> {
    void deleteAllByUserId(Long userId);


    @Modifying
    @Query("DELETE FROM Answer a WHERE a.user.id = :userId AND a.form IN :forms")
    void deleteAnswersByUserIdAndForms(@Param("userId") Long userId, @Param("forms") List<Form> forms);

    @Query(value = "SELECT * FROM answer a " +
        "WHERE a.form_id = :formId AND a.user_id = :userId " +
        "ORDER BY a.created_at DESC LIMIT 1", nativeQuery = true)
    Answer findRecentAnswerByFormIdAndUserId(Long formId, Long userId);
}

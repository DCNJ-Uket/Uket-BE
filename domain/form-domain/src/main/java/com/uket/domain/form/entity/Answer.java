package com.uket.domain.form.entity;

import com.uket.domain.core.entity.BaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import lombok.Getter;

@Getter
public abstract class Answer extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "answer_id")
    private Long id;
    private Long formId;
    private Long userId;
    private String question;
    private String response;

    public Answer(Long formId, Long userId, String question, String response) {
        this.formId = formId;
        this.userId = userId;
        this.question = question;
        this.response = response;
    }

    abstract public void validate();
}

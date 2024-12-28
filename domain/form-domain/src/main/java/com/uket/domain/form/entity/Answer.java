package com.uket.domain.form.entity;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.core.entity.BaseEntity;
import com.uket.domain.form.exception.FormException;
import com.uket.domain.user.entity.Users;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
public class Answer extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "answer_id")
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "form_id")
    private Form form;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id")
    private Users user;

    private String response;

    public Answer(Form form, Users user, String response) {
        this.form = form;
        this.user = user;
        this.response = response;
    }

    public void validate() {
        if(this.form.getFormType().equals(FormType.TEXT)) {

            if (this.response == null)
                throw new FormException(ErrorCode.NOT_FOUND_RESPONSE);
            if (this.form.isOverMaxLength(this.response.length()))
                throw new FormException(ErrorCode.EXCEED_MAX_LENGTH);

        } else if(this.form.getFormType().equals(FormType.DROPDOWN)) {
            if(!this.form.containsInOptions(this.response))
                throw new FormException(ErrorCode.NOT_IN_RANGE);
        }
    }
}

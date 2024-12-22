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
import java.util.List;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

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
                throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
            if (this.form.isOverMaxLength(this.response.length()))
                throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);

        } else if(this.form.getFormType().equals(FormType.DROPDOWN)) {

            int index;
            try {
                index = Integer.parseInt(response);
            } catch(NumberFormatException e) {
                throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
            }
            if(!this.form.containsInOptions(index))
                throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);

        }
    }
}

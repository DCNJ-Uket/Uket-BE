package com.uket.domain.form.entity;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.core.entity.BaseEntity;
import com.uket.domain.event.entity.Events;
import com.uket.domain.form.dto.UserResponseDto;
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
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.AllArgsConstructor;
import lombok.Generated;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.boot.autoconfigure.web.WebProperties.Resources.Chain.Strategy;

@Entity
@NoArgsConstructor
@Getter
@AllArgsConstructor
public class Survey extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "survey_id")
    private Long id;

    @OneToMany(mappedBy = "form_id")
    private List<Form> forms;

    public List<Answer> createAnswers(Users user, List<UserResponseDto> responseDtos) {
        List<Answer> answers = new ArrayList<>();

        Map<Long, Form> formMap = forms.stream().collect(Collectors.toMap(Form::getId, form -> form));
        for(UserResponseDto responseDto : responseDtos) {
            Form f = formMap.get(responseDto.formId());
            if(f == null)
                throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
            answers.add(f.createAnswer(user, responseDto.response()));
        }

        return answers;
    }
}

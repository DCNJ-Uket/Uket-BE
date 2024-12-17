package com.uket.domain.form.entity;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.dto.UserResponseDto;
import com.uket.domain.form.exception.FormException;
import com.uket.domain.user.entity.Users;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class Survey {
    private Long id;
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

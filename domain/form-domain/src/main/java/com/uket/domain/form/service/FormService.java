package com.uket.domain.form.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.dto.FormResponseDto;
import com.uket.domain.form.dto.OptionDto;
import com.uket.domain.form.entity.Answer;
import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.Options;
import com.uket.domain.form.entity.Survey;
import com.uket.domain.form.exception.FormException;
import com.uket.domain.form.repository.AnswerRepository;
import com.uket.domain.form.repository.FormRepository;
import com.uket.domain.form.repository.OptionsRepository;
import com.uket.domain.form.repository.SurveyRepository;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.exception.UserException;
import com.uket.domain.user.repository.UserRepository;
import jakarta.transaction.Transactional;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class FormService {
    private final SurveyRepository surveyRepository;
    private final AnswerRepository answerRepository;
    private final UserRepository userRepository;
    private final FormRepository formRepository;
    private final OptionsRepository optionsRepository;

    public Survey findById(Long surveyId) {
        return surveyRepository.findById(surveyId)
                .orElseThrow(() -> new FormException(ErrorCode.NOT_FOUND_SURVEY));
    }

    public List<Form> findFormsBySurveyId(Long surveyId) {
        return formRepository.findBySurveyId(surveyId);
    }

    public List<OptionDto> findOptionsByFormId(Long formId) {
        List<OptionDto> optionDtos = new ArrayList<>();
        List<Options> options = optionsRepository.findByFormId(formId);
        for(Options option : options) {
            optionDtos.add(OptionDto.from(option));
        }
        return optionDtos;
    }

    @Transactional
    public List<Answer> submitResponse(Long surveyId, Long userId, List<FormResponseDto> responses) {
        Survey survey = surveyRepository.findById(surveyId)
                .orElseThrow(() -> new FormException(ErrorCode.NOT_FOUND_SURVEY));

        Users user = userRepository.findById(userId)
            .orElseThrow(() -> new UserException(ErrorCode.NOT_FOUND_USER));

        List<Answer> answers = createAnswers(survey.getForms(), user, responses);
        answers.forEach(Answer::validate);
        return answerRepository.saveAll(answers);
    }

    private List<Answer> createAnswers(List<Form> forms, Users user, List<FormResponseDto> responses) {
        List<Answer> answers = new ArrayList<>();
        Map<Long, Form> formMap = forms.stream().collect(Collectors.toMap(Form::getId, form -> form));
        for(FormResponseDto res : responses) {
            Form f = formMap.get(res.formId());
            if(f == null)
                throw new FormException(ErrorCode.NOT_FOUND_FORM);
            answers.add(new Answer(f, user, res.response()));
        }
        return answers;
    }
}

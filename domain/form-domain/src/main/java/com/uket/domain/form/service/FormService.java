package com.uket.domain.form.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.form.dto.AnswerDto;
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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class FormService {
    private final SurveyRepository surveyRepository;
    private final AnswerRepository answerRepository;
    private final UserRepository userRepository;
    private final FormRepository formRepository;
    private final OptionsRepository optionsRepository;

    @Transactional(readOnly = true)
    public Survey findById(Long surveyId) {
        return surveyRepository.findById(surveyId)
            .orElseThrow(() -> new FormException(ErrorCode.NOT_FOUND_SURVEY));
    }

    @Transactional(readOnly = true)
    public List<Form> findFormsBySurveyId(Long surveyId) {
        return formRepository.findBySurveyId(surveyId);
    }
    @Transactional(readOnly = true)
    public List<OptionDto> findOptionsByFormId(Long formId) {
        List<OptionDto> optionDtos = new ArrayList<>();
        List<Options> options = optionsRepository.findByFormId(formId);
        for(Options option : options) {
            optionDtos.add(OptionDto.from(option));
        }
        return optionDtos;
    }

    @Transactional(readOnly = true)
    public AnswerDto findAnswerByFormIdAndUserId(Long formId, Long userId, boolean isNecessary) {
        Answer answer = answerRepository.findRecentAnswerByFormIdAndUserId(formId, userId);
        /*
        기존 데이터
        - 필수 응답 여부와 관계 없이, 응답 데이터가 아예 없거나, 응답 내용이 ""일 수 있음

        새로운 데이터
        - 필수 응답인 경우, 응답 데이터가 무조건 존재하고 내용도 제대로 되어있음
        - 필수 응답이 아닌 경우, 응답 데이터는 무조건 존재하지만 응답 내용이 "응답하지 않았습니다"일 수 있음

        1. 응답 데이터가 존재하는가?
        2. 응답 데이터가 존재는 한다면, 응답 내용이 잘못되어 있는가?
         */
        if(answer == null)
            return AnswerDto.noAnswerDto;
        if(answer.getResponse().isEmpty() || answer.getResponse().equals("응답하지 않았습니다"))
            return AnswerDto.noAnswerDto;

        // TODO : 기존 데이터를 싹 날려버린 이후에는 새로운 데이터가 갖춰야할 조건에 대한 예외처리로 수정 필요 ex.하단 주석
        // if(answer == null)
        //     throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);
        // if(isNecessary && answer.getResponse().equals("응답하지 않았습니다"))
        //     throw new FormException(ErrorCode.UNKNOWN_SERVER_ERROR);

        return AnswerDto.from(answer);
    }

    @Transactional
    public List<Answer> submitResponse(Long surveyId, Long userId, List<FormResponseDto> responses) {
        Survey survey = surveyRepository.findById(surveyId)
            .orElseThrow(() -> new FormException(ErrorCode.NOT_FOUND_SURVEY));

        Users user = userRepository.findById(userId)
            .orElseThrow(() -> new UserException(ErrorCode.NOT_FOUND_USER));

        List<FormResponseDto> answerResponses = new ArrayList<>();
        for(FormResponseDto formResponseDto : responses) {
            Form form = formRepository.findById(formResponseDto.formId())
                .orElseThrow(() -> new FormException(ErrorCode.NOT_FOUND_FORM));

            if(!formResponseDto.response().isEmpty()) {
                answerResponses.add(formResponseDto);
            } else {
                if(Boolean.TRUE.equals(form.getIsNecessary())) {
                    throw new FormException(ErrorCode.NOT_FOUND_RESPONSE);
                }
            }
        }

        List<Answer> answers = createAnswers(survey.getForms(), user, answerResponses);
        answers.forEach(Answer::validate);
        return answerRepository.saveAll(answers);
    }

    @Transactional
    public void deleteAnswers(Long userId) {
        answerRepository.deleteAllByUserId(userId);
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

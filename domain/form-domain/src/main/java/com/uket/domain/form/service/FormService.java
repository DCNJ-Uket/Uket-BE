package com.uket.domain.form.service;

import com.uket.domain.form.entity.Survey;
import com.uket.domain.form.exception.FormException;
import com.uket.domain.form.repository.FormRepository;
import com.uket.domain.form.repository.SurveyRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class FormService {
    private final SurveyRepository surveyRepository;

    public Survey findById(int id) {
        return surveyRepository.findById(id)
                .orElseThrow(() -> new FormException(""));
    }
}

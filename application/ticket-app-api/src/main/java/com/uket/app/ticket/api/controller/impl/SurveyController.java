package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.SurveyApi;
import com.uket.app.ticket.api.dto.request.SurveyResponseRequest;
import com.uket.app.ticket.api.dto.response.SurveyAnswerResponse;
import com.uket.domain.form.dto.FormResponseDto;
import com.uket.domain.form.entity.Answer;
import com.uket.domain.form.service.FormService;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class SurveyController implements SurveyApi {

    private final FormService formService;

    @Override
    public ResponseEntity<SurveyAnswerResponse> makeSurveyResponse(Long userId, SurveyResponseRequest request) {
        List<Answer> answers = formService.submitResponse(request.surveyId(), userId, request.responses());
        SurveyAnswerResponse response = SurveyAnswerResponse.from(request.surveyId(), userId, answers);
        return ResponseEntity.ok(response);
    }
}

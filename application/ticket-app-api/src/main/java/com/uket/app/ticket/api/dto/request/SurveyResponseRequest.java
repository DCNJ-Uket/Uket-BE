package com.uket.app.ticket.api.dto.request;

import com.uket.domain.form.dto.FormResponseDto;
import java.util.List;

public record SurveyResponseRequest(
    Long surveyId,

    List<FormResponseDto> responses
) {

}

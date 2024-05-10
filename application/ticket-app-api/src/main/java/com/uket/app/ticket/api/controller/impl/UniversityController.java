package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.UniversityApi;
import com.uket.app.ticket.api.dto.response.CurrentEventResponse;
import com.uket.app.ticket.api.dto.response.ListResponse;
import com.uket.app.ticket.api.dto.response.UniversityResponse;
import com.uket.app.ticket.api.service.UniversityEventService;
import com.uket.domain.event.entity.Events;
import com.uket.domain.university.dto.UniversityDto;
import com.uket.modules.aws.s3.service.S3Service;
import java.time.LocalDate;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class UniversityController implements UniversityApi {

    private final UniversityEventService universityEventService;
    private final S3Service s3Service;

    @Override
    public ResponseEntity<ListResponse<UniversityResponse>> getUniversities() {
        LocalDate now = LocalDate.now();
        List<UniversityDto> universities = universityEventService.getUniversitiesByDate(now);

        List<UniversityResponse> universityResponses = getUniversityResponses(universities);

        ListResponse<UniversityResponse> response = ListResponse.from(universityResponses);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<CurrentEventResponse> getCurrentEventOfUniversity(Long universityId) {

        Events event = universityEventService.getCurrentEventOfUniversity(universityId);

        CurrentEventResponse response = CurrentEventResponse.from(event);
        return ResponseEntity.ok(response);
    }

    private List<UniversityResponse> getUniversityResponses(List<UniversityDto> universities) {
        return universities.stream()
                .map(universityDto -> {
                    String logoUrl = s3Service.getUniversityLogo(universityDto.logoUrl());
                    return UniversityResponse.of(universityDto, logoUrl);
                })
                .toList();
    }
}

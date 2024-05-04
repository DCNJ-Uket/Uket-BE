package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.UniversityApi;
import com.uket.app.ticket.api.dto.response.ListResponse;
import com.uket.app.ticket.api.service.UniversityEventService;
import com.uket.domain.university.dto.UniversityDto;
import java.time.LocalDate;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class UniversityController implements UniversityApi {

    private final UniversityEventService universityEventService;

    @Override
    public ResponseEntity<ListResponse<UniversityDto>> getUniversities() {
        LocalDate now = LocalDate.now();
        List<UniversityDto> universities = universityEventService.getUniversitiesByDate(now);

        ListResponse<UniversityDto> response = ListResponse.from(universities);
        return ResponseEntity.ok(response);
    }
}

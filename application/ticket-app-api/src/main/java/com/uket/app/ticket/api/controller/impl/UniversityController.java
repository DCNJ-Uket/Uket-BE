package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.UniversityApi;
import com.uket.app.ticket.api.dto.response.ListResponse;
import com.uket.domain.university.dto.UniversityDto;
import com.uket.domain.university.service.UniversityService;
import java.time.LocalDate;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class UniversityController implements UniversityApi {

    private final UniversityService universityService;

    @Override
    public ResponseEntity<ListResponse<UniversityDto>> getUniversities() {
        LocalDate now = LocalDate.now();
        List<UniversityDto> universities = universityService.getUniversities(now);

        ListResponse<UniversityDto> response = ListResponse.from(universities);

        return ResponseEntity.ok(response);
    }
}

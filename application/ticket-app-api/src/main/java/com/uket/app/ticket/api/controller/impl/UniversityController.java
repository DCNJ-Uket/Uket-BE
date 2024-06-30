package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.UniversityApi;
import com.uket.app.ticket.api.dto.response.CertifiableUniversityResponse;
import com.uket.app.ticket.api.dto.response.CurrentEventResponse;
import com.uket.app.ticket.api.dto.response.ListResponse;
import com.uket.app.ticket.api.service.CertificationService;
import com.uket.app.ticket.api.util.S3ImageUrlConverter;
import com.uket.app.ticket.api.service.UniversityEventService;
import com.uket.domain.event.dto.BannerDto;
import com.uket.domain.event.entity.Events;
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
    private final CertificationService certificationService;
    private final S3ImageUrlConverter s3ImageUrlConverter;

    @Override
    public ResponseEntity<ListResponse<UniversityDto>> getUniversities() {

        LocalDate now = LocalDate.now();

        List<UniversityDto> universities = s3ImageUrlConverter.getUniversitiesByDate(now);

        ListResponse<UniversityDto> response = ListResponse.from(universities);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<CurrentEventResponse> getCurrentEventOfUniversity(Long universityId) {

        Events event = universityEventService.getCurrentEventOfUniversity(universityId);

        List<BannerDto> banners = s3ImageUrlConverter.getBanners(event);

        CurrentEventResponse response = CurrentEventResponse.of(event, banners);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<ListResponse<CertifiableUniversityResponse>> getCertifiableOfUniversity() {

        List<UniversityDto> certifiableOfUniversity = certificationService.getCertifiableOfUniversity();

        List<CertifiableUniversityResponse> certifiableUniversityResponses = certifiableOfUniversity.stream()
                .map(it -> CertifiableUniversityResponse.of(it.id(), it.name())).toList();

        ListResponse<CertifiableUniversityResponse> response = ListResponse.from(certifiableUniversityResponses);
        return ResponseEntity.ok(response);
    }
}

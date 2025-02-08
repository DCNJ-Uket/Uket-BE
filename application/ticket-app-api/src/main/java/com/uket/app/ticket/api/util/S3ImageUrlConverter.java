package com.uket.app.ticket.api.util;

import com.uket.app.ticket.api.dto.response.ActiveUniversitiesResponse;
import com.uket.app.ticket.api.service.UniversityEventService;
import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.dto.BannerDto;
import com.uket.domain.event.dto.ShowDto;
import com.uket.domain.event.entity.Banner;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.repository.BannerRepository;
import com.uket.domain.event.service.ShowService;
import com.uket.domain.university.dto.UniversityDto;
import com.uket.modules.aws.s3.service.S3Service;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Comparator;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class S3ImageUrlConverter {

    private final S3Service s3Service;
    private final ShowService showService;
    private final UniversityEventService universityEventService;
    private final BannerRepository bannerRepository;

    public List<BannerDto> getBanners(Events event) {

        List<Banner> banners = bannerRepository.findByEvent(event);

        return banners.stream()
                .map(banner -> {
                    String bannerUrl = s3Service.getBannerImage(banner.getPath());
                    return BannerDto.of(banner.getTitle(), bannerUrl, banner.getRedirectUrl());
                }).toList();
    }

    public List<ActiveUniversitiesResponse> getUniversitiesByDate(LocalDate date) {

        List<UniversityDto> universities = universityEventService.getUniversitiesByDate(date);

        return universities.stream()
                .map(universityDto -> {
                    String logoUrl = s3Service.getUniversityLogo(universityDto.logoUrl());
                    Events currentEvent = universityEventService.getCurrentEventOfUniversity(universityDto.id());
                    LocalDateTime firstShowStartDateTime = getFirstShowStartDateTime(currentEvent);

                    return ActiveUniversitiesResponse.builder()
                            .id(universityDto.id())
                            .name(universityDto.name())
                            .eventName(currentEvent.getName())
                            .logoUrl(logoUrl)
                            .startDateTime(firstShowStartDateTime.atZone(ZoneId.of("Asia/Seoul")))
                            .build();
                }).toList();
    }

    private LocalDateTime getFirstShowStartDateTime(Events currentEvent) {
        return showService.findByEventId(currentEvent.getId()).stream()
                .min(Comparator.comparing(ShowDto::startDate))
                .orElseThrow(() -> new BaseException(ErrorCode.NOT_FOUND_SHOW))
                .startDate().toLocalDateTime();
    }
}

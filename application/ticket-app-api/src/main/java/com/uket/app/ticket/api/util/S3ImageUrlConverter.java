package com.uket.app.ticket.api.util;

import com.uket.app.ticket.api.service.UniversityEventService;
import com.uket.domain.event.dto.BannerDto;
import com.uket.domain.event.entity.Banner;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.repository.BannerRepository;
import com.uket.domain.university.dto.UniversityDto;
import com.uket.modules.aws.s3.service.S3Service;
import java.time.LocalDate;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class S3ImageUrlConverter {

    private final S3Service s3Service;
    private final UniversityEventService universityEventService;
    private final BannerRepository bannerRepository;

    public List<BannerDto> getBanners(Events event) {

        List<Banner> banners = bannerRepository.findByEvent(event);

        return banners.stream()
                .map(banner -> {
                    String bannerUrl = s3Service.getBannerImage(banner.getPath());
                    return BannerDto.of(banner.getTitle(), bannerUrl);
                }).toList();
    }

    public List<UniversityDto> getUniversitiesByDate(LocalDate date) {

        List<UniversityDto> universities = universityEventService.getUniversitiesByDate(date);

        return universities.stream()
                .map(universityDto -> {
                    String logoUrl = s3Service.getUniversityLogo(universityDto.logoUrl());
                    return UniversityDto.updateLogoUrl(universityDto, logoUrl);
                }).toList();
    }
}

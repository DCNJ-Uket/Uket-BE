package com.uket.app.ticket.api.dto.response;

import com.uket.domain.event.dto.BannerDto;
import com.uket.domain.event.entity.Events;
import java.time.LocalDate;
import java.util.List;
import lombok.Builder;

@Builder
public record CurrentEventResponse(
        Long id,
        String name,
        LocalDate startDate,
        LocalDate endDate,
        String location,
        List<BannerDto> banners
) {

    public static CurrentEventResponse of(Events event, List<BannerDto> banners) {
        return CurrentEventResponse.builder()
                .id(event.getId())
                .name(event.getName())
                .startDate(event.getStartDate())
                .endDate(event.getEndDate())
                .location(event.getLocation())
                .banners(banners)
                .build();
    }
}

package com.uket.domain.event.dto;

import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.enums.ReservationUserType;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import lombok.Builder;

@Builder
public record ReservationDto(
        Long id,
        ReservationUserType type,
        ZonedDateTime startTime,
        ZonedDateTime endTime,
        Integer reservedCount,
        Integer totalCount
) {
    private static final String ZONE_ID = "Asia/Seoul";

    public static ReservationDto from(Reservation reservation) {
        return ReservationDto.builder()
                .id(reservation.getId())
                .type(reservation.getType())
                .startTime(ZonedDateTime.of(reservation.getStartTime(), ZoneId.of(ZONE_ID)))
                .endTime(ZonedDateTime.of(reservation.getEndTime(), ZoneId.of(ZONE_ID)))
                .reservedCount(reservation.getReservedCount())
                .totalCount(reservation.getTotalCount())
                .build();
    }

    public static ReservationDto from(ReservationQueryDto reservation) {
        return ReservationDto.builder()
                .id(reservation.id())
                .type(reservation.type())
                .startTime(ZonedDateTime.of(reservation.startTime(), ZoneId.of(ZONE_ID)))
                .endTime(ZonedDateTime.of(reservation.endTime(), ZoneId.of(ZONE_ID)))
                .reservedCount(reservation.reservedCount())
                .totalCount(reservation.totalCount())
                .build();
    }
}

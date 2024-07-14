package com.uket.domain.event.dto;

import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.enums.ReservationUserType;
import java.time.LocalDateTime;
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
    public static ReservationDto from(Reservation reservation) {
        return ReservationDto.builder()
                .id(reservation.getId())
                .type(reservation.getType())
                .startTime(ZonedDateTime.of(reservation.getStartTime(), ZoneId.of("Asia/Seoul")))
                .endTime(ZonedDateTime.of(reservation.getEndTime(), ZoneId.of("Asia/Seoul")))
                .reservedCount(reservation.getReservedCount())
                .totalCount(reservation.getTotalCount())
                .build();
    }
}

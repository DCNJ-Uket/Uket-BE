package com.uket.domain.event.dto;

import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.enums.ReservationUserType;
import java.time.LocalDateTime;
import lombok.Builder;

@Builder
public record ReservationDto(
        Long id,
        ReservationUserType type,
        LocalDateTime startTime,
        LocalDateTime endTime,
        Integer reservedCount,
        Integer totalCount
) {
    public static ReservationDto from(Reservation reservation) {
        return ReservationDto.builder()
                .id(reservation.getId())
                .type(reservation.getType())
                .startTime(reservation.getStartTime())
                .endTime(reservation.getEndTime())
                .reservedCount(reservation.getReservedCount())
                .totalCount(reservation.getTotalCount())
                .build();
    }
}

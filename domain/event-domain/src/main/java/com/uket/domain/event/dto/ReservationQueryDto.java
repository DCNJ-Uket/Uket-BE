package com.uket.domain.event.dto;

import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.enums.ReservationUserType;
import java.time.LocalDateTime;
import lombok.Builder;

@Builder
public record ReservationQueryDto(
        Long id,
        ReservationUserType type,
        LocalDateTime startTime,
        LocalDateTime endTime,
        Integer reservedCount,
        Integer totalCount
) {
    public static ReservationQueryDto from(Reservation reservation) {
        return ReservationQueryDto.builder()
                .id(reservation.getId())
                .type(reservation.getType())
                .startTime(reservation.getStartTime())
                .endTime(reservation.getEndTime())
                .reservedCount(reservation.getReservedCount())
                .totalCount(reservation.getTotalCount())
                .build();
    }
}

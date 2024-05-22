package com.uket.domain.event.entity;

import com.uket.domain.core.entity.BaseEntity;
import com.uket.domain.event.enums.ReservationUserType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import java.time.LocalDateTime;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Getter
@Builder
@AllArgsConstructor
@Table(uniqueConstraints = {
        @UniqueConstraint(columnNames = {"show_id", "start_time", "end_time"})
})
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Reservation extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "reservation_id")
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "show_id")
    private Shows show;

    private LocalDateTime startTime;
    private LocalDateTime endTime;
    private Integer reservedCount;
    private Integer totalCount;

    @Enumerated(EnumType.STRING)
    private ReservationUserType type;

    public Boolean increaseReservedCount() {
        if (reservedCount + 1 > totalCount) {
            return false;
        }

        this.reservedCount += 1;
        return true;
    }
}

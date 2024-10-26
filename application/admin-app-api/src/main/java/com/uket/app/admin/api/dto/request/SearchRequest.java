package com.uket.app.admin.api.dto.request;

import com.uket.domain.event.enums.ReservationUserType;
import com.uket.domain.ticket.enums.TicketStatus;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDate;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.lang.Nullable;

public record SearchRequest(
    @Schema(description = "티켓 상태", example = "BEFORE_ENTER")
    @Nullable
    TicketStatus status,
    @Schema(description = "사용자 이름", example = "홍길동")
    @Nullable
    String userName,

    @Schema(description = "사용자 전화 번호", example = "01011112222")
    @Nullable
    String phoneNumber,
    @Schema(description = "공연 날짜", example = "24.09.04")
    @DateTimeFormat(pattern = "yy.MM.dd")
    @Nullable
    LocalDate showDate,
    @Schema(description = "사용자 구분", example = "TICKETING_ALL")
    @Nullable
    ReservationUserType reservationUserType,
    @Schema(description = "티켓 생성 일시", example = "24.06.12")
    @DateTimeFormat(pattern = "yy.MM.dd")
    @Nullable
    LocalDate createdAt,
    @Schema(description = "티켓 수정 일시", example = "24.06.12")
    @DateTimeFormat(pattern = "yy.MM.dd")
    @Nullable
    LocalDate modifiedAt

) {

}

package com.uket.domain.ticket.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum TicketStatus {
    BEFORE_ENTER("예매 완료"),
    FINISH_ENTER("입장 완료"),
    BEFORE_PAYMENT("입금 확인중"),
    RESERVATION_CANCEL("예매 취소"),
    EXPIRED("기간 만료");

    private final String value;
}

package com.uket.domain.ticket.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum TicketStatus {
    BEFORE_ENTER("예매 완료", "예매가 성공적으로 이루어졌습니다. 공연을 즐겨보세요!"),
    FINISH_ENTER("입장 완료", "입장이 이미 완료된 티켓입니다. 재입장은 담당자에게 문의 부탁드립니다."),
    BEFORE_PAYMENT("입금 확인중", "예약은 완료되었으나 아직 해당 티켓 금액이 입금되지 않았습니다."),
    RESERVATION_CANCEL("예매 취소", "예매가 취소되었습니다. 해당 티켓으로 입장하실 수 없습니다."),
    EXPIRED("기간 만료", "이미 기간이 지난 티켓입니다. 날짜를 확인해주세요.");

    private final String value;
    private final String msg;
}

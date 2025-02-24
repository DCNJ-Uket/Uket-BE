package com.uket.modules.redis.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum RedisErrorCode {
    /**
     * Redis Errors
     */
    INVALID_OR_EXPIRED_REFRESH_TOKEN(401, "RD0001", "RefreshToken이 만료되었거나 유효하지 않습니다. 확인 부탁드립니다."),
    ACCESS_TOKEN_NOT_STORED(404, "RD0002", "요청하신 AccessToken이 저장소에 존재하지 않습니다. 확인 부탁드립니다."),
    USER_ID_NOT_FOUND(404, "RD0004", "사용자 ID를 찾을 수 없습니다. 데이터를 확인해 주세요."),
    UNKNOWN_SERVER_ERROR(500,"CM0001", "일시적으로 접속이 원활하지 않습니다. 서버 팀에 문의 부탁드립니다.");

    private final int status;
    private final String code;
    private final String message;
}

package com.uket.modules.redis.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum ErrorCode {
    /**
     * Redis Errors
     */
    INVALID_OR_EXPIRED_REFRESH_TOKEN(401, "RD0001", "RefreshToken이 만료되었거나 유효하지 않습니다. 확인 부탁드립니다."),
    ACCESS_TOKEN_NOT_STORED(404, "RD0002", "요청하신 AccessToken이 저장소에 존재하지 않습니다. 확인 부탁드립니다.");


    private final int status;
    private final String code;
    private final String message;
}

package com.uket.modules.redis.dto.response;

import com.uket.modules.redis.exception.RedisErrorCode;

public record RedisErrorResponse(
    String code,

    String message
) {
    public static RedisErrorResponse of(String code, String message) {
        return new RedisErrorResponse(code, message);
    }

    public static RedisErrorResponse of(RedisErrorCode errorCode) {
        return new RedisErrorResponse(errorCode.getCode(), errorCode.getMessage());
    }
}

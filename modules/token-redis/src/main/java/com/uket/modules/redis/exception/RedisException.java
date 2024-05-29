package com.uket.modules.redis.exception;

import lombok.Getter;

@Getter
public class RedisException extends RuntimeException{
    private final ErrorCode errorCode;
    public RedisException(ErrorCode errorCode) {
        super(errorCode.getMessage());
        this.errorCode = errorCode;
    }
}

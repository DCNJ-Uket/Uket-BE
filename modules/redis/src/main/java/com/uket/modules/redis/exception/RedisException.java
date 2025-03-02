package com.uket.modules.redis.exception;

import lombok.Getter;

@Getter
public class RedisException extends RuntimeException{
    private final RedisErrorCode redisErrorCode;
    public RedisException(RedisErrorCode redisErrorCode) {
        super(redisErrorCode.getMessage());
        this.redisErrorCode = redisErrorCode;
    }
}

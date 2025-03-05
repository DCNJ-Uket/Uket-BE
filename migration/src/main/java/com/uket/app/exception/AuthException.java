package com.uket.app.exception;

public class AuthException extends BaseException {

    public AuthException(ErrorCode errorCode) {
        super(errorCode);
    }
}

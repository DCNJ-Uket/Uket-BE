package com.uket.app.auth.exception;


import com.uket.app.exception.BaseException;
import com.uket.app.exception.ErrorCode;

public class AuthException extends BaseException {

    public AuthException(ErrorCode errorCode) {
        super(errorCode);
    }
}

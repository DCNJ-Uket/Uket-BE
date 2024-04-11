package com.uket.domain.auth.exception;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;

public class ExpiredTokenException extends BaseException {

    public ExpiredTokenException() {
        super(ErrorCode.TOKEN_EXPIRED);
    }
}

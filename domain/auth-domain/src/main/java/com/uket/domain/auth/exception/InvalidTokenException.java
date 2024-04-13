package com.uket.domain.auth.exception;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;

public class InvalidTokenException extends BaseException {

    public InvalidTokenException() {
        super(ErrorCode.INVALID_TOKEN);
    }
}

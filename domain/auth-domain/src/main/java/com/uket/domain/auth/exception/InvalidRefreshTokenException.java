package com.uket.domain.auth.exception;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;

public class InvalidRefreshTokenException extends BaseException {

    public InvalidRefreshTokenException() {
        super(ErrorCode.REFRESH_TOKEN_INVALID);
    }
}

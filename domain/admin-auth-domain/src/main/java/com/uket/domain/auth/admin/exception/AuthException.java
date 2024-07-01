package com.uket.domain.auth.admin.exception;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;

public class AuthException extends BaseException {

    public AuthException(ErrorCode errorCode) {
        super(errorCode);
    }
}

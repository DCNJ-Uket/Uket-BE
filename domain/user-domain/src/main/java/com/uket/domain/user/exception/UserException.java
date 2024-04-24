package com.uket.domain.user.exception;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;

public class UserException extends BaseException {

    public UserException(ErrorCode errorCode) {
        super(errorCode);
    }
}

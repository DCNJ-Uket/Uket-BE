package com.uket.app.domain.user.admin.exception;

import com.uket.app.exception.BaseException;
import com.uket.app.exception.ErrorCode;

public class AdminException extends BaseException {

    public AdminException(ErrorCode errorCode) {
        super(errorCode);
    }
}

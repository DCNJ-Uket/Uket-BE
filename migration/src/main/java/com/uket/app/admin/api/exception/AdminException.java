package com.uket.app.admin.api.exception;

import com.uket.app.exception.BaseException;
import com.uket.app.exception.ErrorCode;

public class AdminException extends BaseException {

    public AdminException(ErrorCode errorCode) {
        super(errorCode);
    }
}

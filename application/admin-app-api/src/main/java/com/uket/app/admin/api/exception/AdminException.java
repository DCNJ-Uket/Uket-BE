package com.uket.app.admin.api.exception;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;

public class AdminException extends BaseException {

    public AdminException(ErrorCode errorCode) {
        super(errorCode);
    }
}

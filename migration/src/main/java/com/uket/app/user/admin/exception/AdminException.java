package com.uket.app.user.admin.exception;

import com.uket.app.global.exception.BaseException;
import com.uket.app.global.exception.ErrorCode;

public class AdminException extends BaseException {

    public AdminException(ErrorCode errorCode) {
        super(errorCode);
    }
}

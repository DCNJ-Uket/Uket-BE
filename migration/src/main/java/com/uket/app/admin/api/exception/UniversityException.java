package com.uket.app.admin.api.exception;

import com.uket.app.exception.BaseException;
import com.uket.app.exception.ErrorCode;

public class UniversityException extends BaseException {

    public UniversityException(ErrorCode errorCode) {
        super(errorCode);
    }

}

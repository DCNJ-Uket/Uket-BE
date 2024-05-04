package com.uket.domain.university.exception;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;

public class UniversityException extends BaseException {

    public UniversityException(ErrorCode errorCode) {
        super(errorCode);
    }
}

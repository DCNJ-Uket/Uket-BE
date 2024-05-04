package com.uket.domain.event.exception;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;

public class EventException extends BaseException {

    public EventException(ErrorCode errorCode) {
        super(errorCode);
    }
}

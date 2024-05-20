package com.uket.domain.ticket.exception;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;

public class TicketException extends BaseException {

    public TicketException(ErrorCode errorCode) {
        super(errorCode);
    }
}

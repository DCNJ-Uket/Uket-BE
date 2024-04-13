package com.uket.domain.auth.exception;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;

public class NotFoundRefreshTokenException extends BaseException {

    public NotFoundRefreshTokenException() {
        super(ErrorCode.NOT_FOUND_REFRESH_TOKEN);
    }
}

package com.uket.domain.form.exception;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;

public class FormException extends BaseException {
    public FormException(ErrorCode errorCode) {
        super(errorCode);
    }
}

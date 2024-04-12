package com.uket.domain.auth.exception;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;

public class TokenCategoryException extends BaseException {

    public TokenCategoryException() {
        super(ErrorCode.NOT_MATCH_CATEGORY);
    }
}

package com.uket.core.dto.response;

import com.uket.core.exception.ErrorCode;

public record ErrorResponse(
        String code,

        String message
) {
    public static ErrorResponse of(String code, String message) {
        return new ErrorResponse(code, message);
    }

    public static ErrorResponse of(ErrorCode errorCode) {
        return new ErrorResponse(errorCode.getCode(), errorCode.getMessage());
    }
}

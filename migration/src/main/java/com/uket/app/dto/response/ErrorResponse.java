package com.uket.app.dto.response;

import com.uket.app.exception.ErrorCode;

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

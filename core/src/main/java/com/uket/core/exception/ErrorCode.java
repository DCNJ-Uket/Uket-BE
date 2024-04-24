package com.uket.core.exception;


import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum ErrorCode {
    /**
     * Common Errors
     */
    UNKNOWN_SERVER_ERROR("CM0001", "일시적으로 접속이 원활하지 않습니다. 서버 팀에 문의 부탁드립니다."),
    INVALID_INPUT_VALUE("CM0002", "유효하지 않은 입력입니다."),
    METHOD_NOT_ALLOWED("CM0003", "허가되지 않은 메서드입니다."),
    INVALID_DATE("CM0004", "유효하지 않은 연 또는 월입니다."),
    /**
     * Auth Related Errors
     */
    AUTHENTICATION_FAILED("AU0001", "인증에 실패하였습니다."),
    TOKEN_AUTHENTICATION_FAILED("AU0002", "토큰 인증에 실패하였습니다."),
    AUTHORIZATION_FAILED("AU0003", "접근 권한이 없습니다."),
    TOKEN_EXPIRED("AU0004", "만료된 토큰입니다."),
    TOKEN_NOT_EXPIRED("AU0005", "아직 토큰이 만료되지 않았습니다."),
    INVALID_TOKEN("AU0006", "유효하지 않은 토큰입니다."),
    INVALID_PLATFORM("AU0007", "유효하지 않은 플랫폼입니다."),
    NOT_MATCH_CATEGORY("AU0008", "올바르지 않은 유형의 토큰입니다."),
    FAIL_REQUEST_TO_OAUTH2("AU0009", "OAuth2 요청이 실패했습니다."),
    /**
     * User Errors
     */
    NOT_FOUND_USER("US0001", "해당 사용자를 찾을 수 없습니다."),
    ALREADY_EXIST_USER("US0002", "이미 가입된 사용자입니다.");

    private final String code;
    private final String message;
}


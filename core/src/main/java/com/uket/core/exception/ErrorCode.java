package com.uket.core.exception;


import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum ErrorCode {
    /**
     * Common Errors
     */
    UNKNOWN_SERVER_ERROR(500,"CM0001", "일시적으로 접속이 원활하지 않습니다. 서버 팀에 문의 부탁드립니다."),
    INVALID_INPUT_VALUE(400,"CM0002", "유효하지 않은 입력입니다."),
    INVALID_DATE(400,"CM0003", "유효하지 않은 연 또는 월입니다."),
    METHOD_NOT_ALLOWED(405,"CM0004", "허가되지 않은 메서드입니다."),

    /**
     * Auth Related Errors
     */
    NOT_MATCH_CATEGORY(400,"AU0001", "올바르지 않은 유형의 토큰입니다."),
    FAIL_REQUEST_TO_OAUTH2(400,"AU0002", "OAuth2 요청이 실패했습니다."),
    AUTHENTICATION_FAILED(401,"AU0003", "인증에 실패하였습니다."),
    TOKEN_AUTHENTICATION_FAILED(401,"AU0004", "토큰 인증에 실패하였습니다."),
    INVALID_TOKEN(401,"AU0005", "유효하지 않은 토큰입니다."),
    INVALID_PLATFORM(401,"AU0006", "유효하지 않은 플랫폼입니다."),
    AUTHORIZATION_FAILED(403,"AU0007", "접근 권한이 없습니다."),
    TOKEN_EXPIRED(403,"AU0008", "만료된 토큰입니다."),
    TOKEN_NOT_EXPIRED(403,"AU0009", "아직 토큰이 만료되지 않았습니다."),

    /**
     * User Errors
     */
    NOT_FOUND_USER(404,"US0001", "해당 사용자를 찾을 수 없습니다."),
    ALREADY_EXIST_USER(409,"US0002", "이미 가입된 사용자입니다."),
    NOT_REGISTERED_USER(400,"US0003", "가입되지 않은 사용자입니다. 회원가입 후 이용해주세요"),

    /**
     * University Errors
     */
    NOT_FOUND_UNIVERSITY(404,"UN0001", "해당 대학을 찾을 수 없습니다."),
    NOT_MATCH_UNIVERSITY_EMAIL(400,"UN0002", "대학 이메일 정보가 잘못되었습니다."),

    /**
     * Events Errors
     */
    NOT_FOUND_EVENT(404,"EV0001", "해당 축제를 찾을 수 없습니다."),
    NOT_FOUND_CURRENT_EVENT(404,"EV0002", "진행중인 축제를 찾을 수 없습니다.");

    private final int status;
    private final String code;
    private final String message;
}


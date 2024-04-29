package com.uket.app.ticket.api.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;

public record UserRegisterRequest(
        @Schema(description = "입금자명", example = "홍길동")
        String depositorName,
        @Schema(description = "전화번호", example = "01012341234")
        String phoneNumber,
        @Schema(description = "대학 이름", example = "konkuk")
        String university,
        @Schema(description = "학과", example = "컴퓨터공학부")
        String studentMajor,
        @Schema(description = "학번", example = "202412345")
        String studentCode
) {

}

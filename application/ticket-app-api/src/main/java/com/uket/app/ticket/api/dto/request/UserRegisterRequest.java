package com.uket.app.ticket.api.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotNull;
import lombok.Builder;

@Builder
public record UserRegisterRequest(
        @Schema(description = "입금자명", example = "홍길동")
        @NotNull(message = "depositorName 은 null 일 수 없습니다.")
        String depositorName,
        @Schema(description = "전화번호", example = "01012341234")
        @NotNull(message = "phoneNumber 는 null 일 수 없습니다.")
        String phoneNumber,
        @Schema(description = "대학 이름", example = "건국대학교")
        String university,
        @Schema(description = "학교 이메일", example = "abc1234@konkuk.ac.kr")
        @Email(message = "이메일 형식이 올바르지 않습니다.")
        String universityEmail,
        @Schema(description = "학과", example = "컴퓨터공학부")
        String studentMajor,
        @Schema(description = "학번", example = "202412345")
        String studentCode
) {

}

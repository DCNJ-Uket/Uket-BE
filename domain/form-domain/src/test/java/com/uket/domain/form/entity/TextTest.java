package com.uket.domain.form.entity;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.uket.domain.form.exception.FormException;
import org.junit.jupiter.api.Test;

public class TextTest {

    @Test
    void 텍스트_응답에_대한_검증_성공() {
        //given
        Form form = Form.builder()
                .id(1L)
                .formType(FormType.TEXT)
                .maxLength(500)
                .build();

        Answer answer = Answer.builder()
                .id(1L)
                .form(form)
                .response("응답입니다.")
                .build();

        //when

        //then
        assertDoesNotThrow(answer::validate);
    }

    @Test
    void 텍스트_응답에_대한_검증_실패_길이초과() {
        //given
        Form form = Form.builder()
                .id(1L)
                .formType(FormType.TEXT)
                .maxLength(3)
                .build();

        Answer answer = Answer.builder()
                .id(1L)
                .form(form)
                .response("응답입니다.")
                .build();

        //when

        //then
        assertThrows(FormException.class, answer::validate);
    }

    @Test
    void 텍스트_응답에_대한_검증_실패_값이_없음() {
        //given
        Form form = Form.builder()
                .id(1L)
                .formType(FormType.TEXT)
                .maxLength(3)
                .build();

        Answer answer = Answer.builder()
                .id(1L)
                .form(form)
                .response(null)
                .build();

        //when

        //then
        assertThrows(FormException.class, answer::validate);
    }

}

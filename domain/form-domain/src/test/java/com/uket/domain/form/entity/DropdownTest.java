package com.uket.domain.form.entity;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.uket.domain.form.exception.FormException;
import java.util.ArrayList;
import org.junit.jupiter.api.Test;

public class DropdownTest {

    @Test
    void 드랍다운_응답에_대한_성공() {
        //given
        Form form = Form.builder()
                .id(1L)
                .formType(FormType.DROPDOWN)
                .options(new ArrayList<>())
                .build();

        Option option = Option.builder()
                .id(1L)
                .form(form)
                .value("이00")
                .build();
        form.getOptions().add(option);

        Answer answer = Answer.builder()
                .id(1L)
                .form(form)
                .response("0")
                .build();

        //when

        //then
        assertDoesNotThrow(answer::validate);
    }

    @Test
    void 드랍다운_응답에_대한_실패_음수값() {
        //given
        Form form = Form.builder()
                .id(1L)
                .formType(FormType.DROPDOWN)
                .options(new ArrayList<>())
                .build();

        Option option = Option.builder()
                .id(1L)
                .form(form)
                .value("이00")
                .build();
        form.getOptions().add(option);

        Answer answer = Answer.builder()
                .id(1L)
                .form(form)
                .response("-1")
                .build();

        //when

        //then
        assertThrows(FormException.class, answer::validate);
    }

    @Test
    void 드랍다운_응답에_대한_실패_초과() {
        //given
        Form form = Form.builder()
                .id(1L)
                .formType(FormType.DROPDOWN)
                .options(new ArrayList<>())
                .build();

        Option option = Option.builder()
                .id(1L)
                .form(form)
                .value("이00")
                .build();
        form.getOptions().add(option);

        Answer answer = Answer.builder()
                .id(1L)
                .form(form)
                .response("2")
                .build();

        //when

        //then
        assertThrows(FormException.class, answer::validate);
    }
}

package com.uket.app.admin.api.aop;

import com.uket.app.admin.api.dto.response.CustomPageResponse;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface ApplyMasking {
    Class<?> typeValue() default CustomPageResponse.class;
    Class<?> genericTypeValue() default Void.class;
}

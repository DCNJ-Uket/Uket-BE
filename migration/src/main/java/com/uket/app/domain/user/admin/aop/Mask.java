package com.uket.app.domain.user.admin.aop;

import com.uket.app.domain.user.admin.enums.MaskingType;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.ANNOTATION_TYPE, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
public @interface Mask {
    MaskingType type();

}

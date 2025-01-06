package com.uket.app.admin.api.aop;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.uket.app.admin.api.enums.MaskingType;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.ANNOTATION_TYPE, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
public @interface Mask {
    MaskingType type();

}

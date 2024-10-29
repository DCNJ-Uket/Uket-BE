package com.uket.app.admin.api.aop;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;
import java.lang.reflect.Field;

import java.util.List;

@Aspect
@Component
public class MaskingAspect {
    @Around("@annotation(com.uket.app.admin.api.aop.ApplyMasking)")
    public Object applyMaskingAspect(ProceedingJoinPoint joinPoint) throws Throwable {
        Object response = joinPoint.proceed();

        if (response instanceof List<?>) {
            for (Object item : (List<?>) response) {
                applyMaskingForDto(item);
            }
        } else {
            applyMaskingForDto(response);
        }
        return response;
    }

    private void applyMaskingForDto(Object dto) throws IllegalAccessException {
        if (dto == null) return;

        Field[] fields = dto.getClass().getDeclaredFields();
        for (Field field : fields) {
            if (field.isAnnotationPresent(Mask.class) && field.getType().equals(String.class)) {
                field.setAccessible(true);
                Mask mask = field.getAnnotation(Mask.class);
                String maskedValue = MaskingUtil.MaskingOf(mask.type(), (String) field.get(dto));
                field.set(dto, maskedValue);
            }
        }
    }
}


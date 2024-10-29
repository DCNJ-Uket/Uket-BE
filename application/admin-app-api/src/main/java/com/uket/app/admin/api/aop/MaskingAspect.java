package com.uket.app.admin.api.aop;

import com.uket.app.admin.api.dto.response.CustomPageResponse;
import com.uket.app.admin.api.dto.response.LiveEnterUserResponse;
import com.uket.app.admin.api.dto.response.TicketResponse;
import com.uket.app.admin.api.exception.AdminException;
import com.uket.core.exception.ErrorCode;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import java.util.List;

@Aspect
@Component
public class MaskingAspect {

    @Around("@annotation(applyMasking)")
    public Object applyMaskingAspect(ProceedingJoinPoint joinPoint, ApplyMasking applyMasking) throws Throwable {
        Object response = joinPoint.proceed();


        if (response instanceof ResponseEntity<?> responseEntity) {
            Object body = responseEntity.getBody();

            if (body instanceof CustomPageResponse<?>) {
                CustomPageResponse<?> maskedResponse = (CustomPageResponse<?>) applyMaskingUtil(applyMasking.typeValue(), body);
                return ResponseEntity.status(responseEntity.getStatusCode()).headers(responseEntity.getHeaders()).body(maskedResponse);
            }
        }

        throw new AdminException(ErrorCode.NOT_REGISTERED_TICKET_MASKING_TYPE);
    }


    private static <T> Object applyMaskingUtil(Class<T> clazz, Object response)
        throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {
        if (response instanceof CustomPageResponse<?> customPageResponse) {
            List<?> originalList = customPageResponse.content();
            List<?> maskedList = applyMaskingUtilForList(clazz, originalList);

            return new CustomPageResponse<>(
                maskedList,
                customPageResponse.pageNumber(),
                customPageResponse.pageSize(),
                customPageResponse.first(),
                customPageResponse.last(),
                customPageResponse.totalElements(),
                customPageResponse.totalPages(),
                customPageResponse.empty()
            );
        }
        throw new AdminException(ErrorCode.NOT_REGISTERED_TICKET_MASKING_TYPE);
    }

    private static <T> List<T> applyMaskingUtilForList(Class<T> clazz, List<?> responseList)
        throws NoSuchMethodException, InvocationTargetException, InstantiationException, IllegalAccessException {
        List<T> maskedList = new ArrayList<>();

        for (Object item : responseList) {
            if (clazz.isInstance(item)) {
                T maskedItem = applyMaskingUtilForDto(clazz, item); // 각 객체에 대해 마스킹 적용
                maskedList.add(maskedItem);
            } else {
                maskedList.add((T) item);
            }
        }

        return maskedList;
    }

    private static <T> T applyMaskingUtilForDto(Class<T> clazz, Object response)
        throws NoSuchMethodException, InvocationTargetException, InstantiationException, IllegalAccessException {
        if (response instanceof TicketResponse ticketResponse) {
            return (T) ticketResponse.withMaskedValues();
        } else if (response instanceof LiveEnterUserResponse liveEnterUserResponse) {
            return (T) liveEnterUserResponse.withMaskedValues();
        }

        throw new AdminException(ErrorCode.NOT_REGISTERED_TICKET_MASKING_TYPE);
    }

}



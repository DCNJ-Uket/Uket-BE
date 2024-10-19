package com.uket.app.admin.api.aop;

import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;
import io.github.bucket4j.Bandwidth;
import io.github.bucket4j.Bucket;
import java.time.Duration;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class LimitRequestAspect {

    private final Bucket bucket;

    public LimitRequestAspect() {
        Bandwidth limit = Bandwidth.simple(100, Duration.ofMinutes(1));
        this.bucket = Bucket.builder()
                .addLimit(limit)
                .build();
    }

    @Before("@annotation(com.uket.app.admin.api.aop.LimitRequest)")
    public void limitRequest() {
        if(!bucket.tryConsume(1)){
            throw new BaseException(ErrorCode.TOO_MANY_REQUEST);
        }
    }
}

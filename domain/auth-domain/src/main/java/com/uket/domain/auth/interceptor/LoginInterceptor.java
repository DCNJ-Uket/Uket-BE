package com.uket.domain.auth.interceptor;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.uket.core.dto.response.ErrorResponse;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.exception.AuthException;
import com.uket.domain.auth.validator.TokenValidator;
import com.uket.modules.jwt.auth.JwtAuthTokenUtil;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;

@Slf4j
@Component
@RequiredArgsConstructor
public class LoginInterceptor implements HandlerInterceptor {

    private final TokenValidator tokenValidator;
    private final ObjectMapper objectMapper;

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler)
            throws Exception {
        if (isPreflight(request) || isSwaggerRequest(request)) {
            return true;
        }

        String token = AuthorizationExtractor.extractAccessToken(request);
        try{
            tokenValidator.validateRegistered(token);
        }catch (AuthException exception) {
            ErrorCode errorCode =  exception.getErrorCode();
            writeErrorResponse(response, errorCode);
            log.warn("[AuthException] {}: {}", errorCode.getCode(), errorCode.getMessage(), exception);
            return false;
        }
        return true;
    }

    private void writeErrorResponse(HttpServletResponse response, ErrorCode errorCode) throws IOException {
        response.setCharacterEncoding(StandardCharsets.UTF_8.name());
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
        response.getWriter()
                .write(objectMapper.writeValueAsString(ErrorResponse.of(errorCode)));
    }

    private boolean isSwaggerRequest(HttpServletRequest request) {
        String uri = request.getRequestURI();
        return uri.contains("swagger") || uri.contains("api-docs") || uri.contains("webjars");
    }

    private boolean isPreflight(HttpServletRequest request) {
        return request.getMethod().equals(HttpMethod.OPTIONS.toString());
    }
}

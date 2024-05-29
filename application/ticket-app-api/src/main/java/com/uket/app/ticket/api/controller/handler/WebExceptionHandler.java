package com.uket.app.ticket.api.controller.handler;

import com.uket.core.dto.response.ErrorResponse;
import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.exception.AuthException;
import com.uket.domain.user.exception.UserException;
import com.uket.modules.redis.dto.response.RedisErrorResponse;
import com.uket.modules.redis.exception.RedisErrorCode;
import com.uket.modules.redis.exception.RedisException;
import com.uket.modules.slack.dto.ErrorReportDto;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.ConstraintViolationException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.CharBuffer;
import java.security.InvalidParameterException;
import java.util.Enumeration;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.ServletRequestBindingException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

@Slf4j
@RequiredArgsConstructor
@RestControllerAdvice
public class WebExceptionHandler {

    private final ApplicationEventPublisher eventPublisher;

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorResponse> unKnownException(HttpServletRequest request,
        Exception exception) {
        return handleUnhandledException(request, exception);
    }

    @ExceptionHandler(AuthException.class)
    ResponseEntity<ErrorResponse> handleAuthException(HttpServletRequest request,
        AuthException exception) {
        ErrorCode errorCode = exception.getErrorCode();
        if (errorCode == ErrorCode.UNKNOWN_SERVER_ERROR) {
            return handleUnhandledException(request, exception);
        }

        log.warn("[AuthException] {}: {}", errorCode.getCode(), errorCode.getMessage(), exception);
        return ResponseEntity
            .status(HttpStatus.valueOf(errorCode.getStatus()))
            .body(ErrorResponse.of(errorCode));
    }

    @ExceptionHandler(UserException.class)
    ResponseEntity<ErrorResponse> handleAuthException(HttpServletRequest request,
        UserException exception) {
        ErrorCode errorCode = exception.getErrorCode();
        if (errorCode == ErrorCode.UNKNOWN_SERVER_ERROR) {
            return handleUnhandledException(request, exception);
        }

        log.warn("[UserException] {}: {}", errorCode.getCode(), errorCode.getMessage(), exception);
        return ResponseEntity
            .status(HttpStatus.valueOf(errorCode.getStatus()))
            .body(ErrorResponse.of(errorCode));
    }

    @ExceptionHandler(BaseException.class)
    ResponseEntity<ErrorResponse> handleBaseException(HttpServletRequest request,
        BaseException exception) {
        ErrorCode errorCode = exception.getErrorCode();
        if (errorCode == ErrorCode.UNKNOWN_SERVER_ERROR) {
            return handleUnhandledException(request, exception);
        }

        log.warn("[BaseException] {}: {}", errorCode.getCode(), errorCode.getMessage(), exception);
        return ResponseEntity
            .status(HttpStatus.valueOf(errorCode.getStatus()))
            .body(ErrorResponse.of(errorCode));
    }

    @ExceptionHandler(RedisException.class)
    ResponseEntity<RedisErrorResponse> handleRedisException(HttpServletRequest request,
        RedisException exception) {
        RedisErrorCode redisErrorCode = exception.getRedisErrorCode();

        log.warn("[RedisException] {}: {}", redisErrorCode.getCode(), redisErrorCode.getMessage(),
            exception);

        return ResponseEntity
            .status(HttpStatus.valueOf(redisErrorCode.getStatus()))
            .body(RedisErrorResponse.of(redisErrorCode));
    }

    @ExceptionHandler(Throwable.class)
    ResponseEntity<ErrorResponse> handleUnhandledException(HttpServletRequest request,
        Throwable exception) {

        StringBuilder dump = dumpRequest(request).append("\n ")
            .append(getStackTraceAsString(exception));

        log.error("[UnhandledException] {} \n", dump);

        eventPublisher.publishEvent(new ErrorReportDto(exception.getMessage(), dump.toString()));
        return ResponseEntity
            .internalServerError()
            .body(ErrorResponse.of(ErrorCode.UNKNOWN_SERVER_ERROR));
    }

    @ExceptionHandler(value = {
        HttpMessageNotReadableException.class,
        InvalidParameterException.class,
        ServletRequestBindingException.class,
        MethodArgumentNotValidException.class,
        ConstraintViolationException.class,
        MethodArgumentTypeMismatchException.class,
    })
    ResponseEntity<ErrorResponse> handleValidateException(HttpServletRequest request,
        Exception exception) {

        log.warn("[InvalidParameterException]", exception);
        return ResponseEntity
            .badRequest()
            .body(ErrorResponse.of(ErrorCode.INVALID_INPUT_VALUE));
    }

    private String getStackTraceAsString(Throwable throwable) {
        StringWriter stringWriter = new StringWriter();
        throwable.printStackTrace(new PrintWriter(stringWriter));
        return stringWriter.toString();
    }

    private StringBuilder dumpRequest(HttpServletRequest request) {
        StringBuilder dump = new StringBuilder("HttpRequest Dump:")
            .append("\n  Remote Addr   : ").append(request.getRemoteAddr())
            .append("\n  Protocol      : ").append(request.getProtocol())
            .append("\n  Request Method: ").append(request.getMethod())
            .append("\n  Request URI   : ").append(request.getRequestURI())
            .append("\n  Query String  : ").append(request.getQueryString())
            .append("\n  Parameters    : ");

        Enumeration<String> parameterNames = request.getParameterNames();
        while (parameterNames.hasMoreElements()) {
            String name = parameterNames.nextElement();
            dump.append("\n    ").append(name).append('=');
            String[] parameterValues = request.getParameterValues(name);
            for (String value : parameterValues) {
                dump.append(value);
            }
        }

        dump.append("\n  Headers       : ");
        Enumeration<String> headerNames = request.getHeaderNames();
        while (headerNames.hasMoreElements()) {
            String name = headerNames.nextElement();
            dump.append("\n    ").append(name).append(":");
            Enumeration<String> headerValues = request.getHeaders(name);
            while (headerValues.hasMoreElements()) {
                dump.append(headerValues.nextElement());
            }
        }

        dump.append("\n  Body       : ");
        if (request.getContentType() != null && request.getContentType()
            .contains("application/x-www-form-urlencoded")) {
            dump.append("\n    ")
                .append("Body is not readable for 'application/x-www-form-urlencoded' type or has been read");
        } else {
            try {
                dump.append("\n    ").append(readableToString(request.getReader()));
            } catch (IOException ex) {
                dump.append("\n    ").append("NOT_READABLE");
            } catch (IllegalStateException ex) {
                dump.append("\n    ").append("BODY_ALREADY_READ");
            }
        }

        return dump;
    }

    public String readableToString(Readable readable) throws IOException {
        StringBuilder stringBuilder = new StringBuilder();
        CharBuffer buffer = CharBuffer.allocate(1024);

        while (readable.read(buffer) != -1) {
            buffer.flip();
            stringBuilder.append(buffer);
            buffer.clear();
        }
        return stringBuilder.toString();
    }
}

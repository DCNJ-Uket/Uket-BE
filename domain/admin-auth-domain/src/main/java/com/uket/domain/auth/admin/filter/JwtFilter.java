package com.uket.domain.auth.admin.filter;

import static com.uket.modules.jwt.constants.JwtValues.JWT_AUTHORIZATION_HEADER;
import static com.uket.modules.jwt.constants.JwtValues.JWT_AUTHORIZATION_VALUE_PREFIX;
import static com.uket.modules.jwt.constants.JwtValues.JWT_PAYLOAD_VALUE_ACCESS;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.uket.core.dto.response.ErrorResponse;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.admin.validator.TokenValidator;
import com.uket.domain.auth.admin.exception.AuthException;
import com.uket.domain.user.dto.UserDto;
import com.uket.domain.user.enums.UserRole;
import com.uket.modules.jwt.util.JwtAuthTokenUtil;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.MediaType;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

@Component
@Slf4j
@RequiredArgsConstructor
public class JwtFilter extends OncePerRequestFilter {

    private final JwtAuthTokenUtil jwtAuthTokenUtil;
    private final TokenValidator tokenValidator;
    private final ObjectMapper objectMapper;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response,
            FilterChain filterChain) throws ServletException, IOException {
        String accessToken = request.getHeader(JWT_AUTHORIZATION_HEADER);

        if (accessToken == null) {
            filterChain.doFilter(request, response);
            return;
        }
        accessToken = accessToken.replace(JWT_AUTHORIZATION_VALUE_PREFIX, "");

        if (validateAccessToken(response,accessToken)) {
            Authentication authentication = new JWTTokenAuthentication(accessToken, generateUserDto(accessToken));
            SecurityContextHolder.getContext().setAuthentication(authentication);
            filterChain.doFilter(request, response);
        }
    }

    private boolean validateAccessToken(HttpServletResponse response,String accessToken) throws IOException {
        try {
            tokenValidator.validateExpiredToken(accessToken);
            tokenValidator.validateTokenSignature(accessToken);
            tokenValidator.validateTokenCategory(JWT_PAYLOAD_VALUE_ACCESS, accessToken);
            tokenValidator.validateRole(UserRole.ROLE_ADMIN, accessToken);
        } catch (AuthException exception) {
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

    private UserDto generateUserDto(String accessToken) {
        Long userId = jwtAuthTokenUtil.getId(accessToken);
        String name = jwtAuthTokenUtil.getName(accessToken);
        String role = jwtAuthTokenUtil.getRole(accessToken);

        return UserDto.builder()
                .userId(userId)
                .name(name)
                .role(role)
                .build();
    }
}

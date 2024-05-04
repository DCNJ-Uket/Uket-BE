package com.uket.domain.auth.config.register;

import com.uket.modules.jwt.auth.JwtAuthTokenUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.MethodParameter;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.support.WebDataBinderFactory;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.method.support.ModelAndViewContainer;

@Slf4j
@Component
@RequiredArgsConstructor
public class IsRegisteredArgumentResolver implements HandlerMethodArgumentResolver {

    private final JwtAuthTokenUtil jwtAuthTokenUtil;

    @Override
    public boolean supportsParameter(MethodParameter parameter) {
        return parameter.hasParameterAnnotation(IsRegistered.class);
    }

    @Override
    public Object resolveArgument(MethodParameter parameter, ModelAndViewContainer mavContainer,
            NativeWebRequest webRequest, WebDataBinderFactory binderFactory) throws Exception {
        String accessToken = (String) SecurityContextHolder.getContext().getAuthentication().getCredentials();
        return jwtAuthTokenUtil.isRegistered(accessToken);
    }
}

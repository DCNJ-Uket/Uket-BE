package com.uket.domain.auth.config;

import com.uket.domain.auth.config.register.IsRegisteredArgumentResolver;
import com.uket.domain.auth.config.userid.LoginUserArgumentResolver;
import com.uket.domain.auth.interceptor.LoginInterceptor;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
@RequiredArgsConstructor
public class AuthConfig implements WebMvcConfigurer {

    private final LoginInterceptor loginInterceptor;
    private final LoginUserArgumentResolver loginUserArgumentResolver;
    private final IsRegisteredArgumentResolver isRegisteredArgumentResolver;

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(loginInterceptor)
                .excludePathPatterns("/api/v1/dev/**")
                .excludePathPatterns("/api/v1/auth/**","/api/v1/users/register")
                .excludePathPatterns("/swagger-resources/**", "/swagger-ui/**", "/v3/api-docs", "/error");
    }

    @Override
    public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
        resolvers.add(loginUserArgumentResolver);
        resolvers.add(isRegisteredArgumentResolver);
        WebMvcConfigurer.super.addArgumentResolvers(resolvers);
    }
}

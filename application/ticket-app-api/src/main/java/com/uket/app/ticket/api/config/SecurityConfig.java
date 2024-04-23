package com.uket.app.ticket.api.config;

import com.uket.app.ticket.api.filter.JwtFilter;
import com.uket.app.ticket.api.handler.CustomSuccessHandler;
import com.uket.domain.auth.service.CustomOAuth2UserService;
import com.uket.domain.auth.validator.TokenValidator;
import com.uket.jwtprovider.auth.JwtAuthTokenUtil;
import java.util.Collections;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.annotation.web.configurers.HeadersConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.oauth2.client.web.OAuth2LoginAuthenticationFilter;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.web.cors.CorsConfiguration;

@Configuration
@EnableWebSecurity
@RequiredArgsConstructor
public class SecurityConfig {

    private static final String ALLOWED_METHOD_NAMES = "GET,HEAD,POST,PUT,DELETE,TRACE,OPTIONS,PATCH";
    private final CustomOAuth2UserService customOAuth2UserService;
    private final JwtAuthTokenUtil jwtAuthTokenUtil;
    private final TokenValidator tokenValidator;
    private final CustomSuccessHandler customSuccessHandler;

    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration configuration)
            throws Exception {

        return configuration.getAuthenticationManager();
    }

    @Bean
    public BCryptPasswordEncoder bCryptPasswordEncoder() {
        return new BCryptPasswordEncoder();
    }

    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http)
            throws Exception {

        http
                .cors(cors -> cors.configurationSource(request -> {

                    CorsConfiguration configuration = new CorsConfiguration();

                    configuration.setAllowedMethods(List.of(ALLOWED_METHOD_NAMES.split(",")));
                    configuration.setAllowedOriginPatterns(Collections.singletonList("*"));
                    configuration.setAllowedHeaders(Collections.singletonList("*"));
                    configuration.setMaxAge(3600L);

                    configuration.setExposedHeaders(List.of(
                            HttpHeaders.AUTHORIZATION
                    ));

                    return configuration;
                }))
                .csrf(AbstractHttpConfigurer::disable)
                .formLogin(AbstractHttpConfigurer::disable)
                .httpBasic(AbstractHttpConfigurer::disable)

                .addFilterAfter(new JwtFilter(jwtAuthTokenUtil, tokenValidator), OAuth2LoginAuthenticationFilter.class)

                .oauth2Login(oauth2 -> oauth2
                        .userInfoEndpoint(userInfoEndpointConfig -> userInfoEndpointConfig
                                .userService(customOAuth2UserService))
                        .successHandler(customSuccessHandler)
                )
                .authorizeHttpRequests(registry ->
                        registry.requestMatchers("/favicon.ico").permitAll()
                                .requestMatchers("/error").permitAll()

                )
                .authorizeHttpRequests(registry -> registry // actuator, rest docs 경로, 실무에서는 상황에 따라 적절한 접근제어 필요
                        .requestMatchers("/actuator/*").permitAll()
                        .requestMatchers("/swagger-ui.html").permitAll()
                        .requestMatchers("/swagger-ui/**").permitAll()
                        .requestMatchers("/v3/api-docs/**").permitAll()
                )
                .authorizeHttpRequests(registry -> registry
                        .requestMatchers("/api/v1/auth").permitAll()
                        .requestMatchers("/api/v1/auth/**").permitAll()
                )
                .authorizeHttpRequests(registry -> registry
                        .anyRequest().authenticated())

                .sessionManagement(session -> session
                        .sessionCreationPolicy(SessionCreationPolicy.STATELESS))

                .headers(headers ->
                        headers.frameOptions(HeadersConfigurer.FrameOptionsConfig::disable)
                );

        return http.build();
    }
}

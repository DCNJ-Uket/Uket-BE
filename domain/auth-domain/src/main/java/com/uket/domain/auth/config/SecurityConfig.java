package com.uket.domain.auth.config;

import com.uket.domain.auth.filter.JwtAccessDeniedHandler;
import com.uket.domain.auth.filter.JwtAuthenticationEntryPoint;
import com.uket.domain.auth.filter.JwtFilter;
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
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;

@Configuration
@EnableWebSecurity
@RequiredArgsConstructor
public class SecurityConfig {

    private static final String ALLOWED_METHOD_NAMES = "GET,HEAD,POST,PUT,DELETE,TRACE,OPTIONS,PATCH";
    private final JwtFilter jwtFilter;

    private final JwtAccessDeniedHandler accessDeniedHandler;
    private final JwtAuthenticationEntryPoint entryPoint;

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

                .addFilterBefore(jwtFilter, UsernamePasswordAuthenticationFilter.class)
                .exceptionHandling(exceptionHandlerManagement ->
                        exceptionHandlerManagement
                                .authenticationEntryPoint(entryPoint)
                                .accessDeniedHandler(accessDeniedHandler))

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
                        .requestMatchers("/api/v1/universities/**").permitAll()
                        .requestMatchers("/api/v1/events/{id}/shows").permitAll()
                )
                .authorizeHttpRequests(registry -> registry
                        .requestMatchers("/api/v1/universities").permitAll()
                        .requestMatchers("/api/v1/universities/{id}/event").permitAll()
                )
                .authorizeHttpRequests(registry -> registry
                        .requestMatchers("/api/v1/events/{id}/shows").permitAll()
                )
                .authorizeHttpRequests(registry -> registry
                        .requestMatchers("/api/v1/dev/token").permitAll()
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

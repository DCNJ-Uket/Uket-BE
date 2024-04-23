package com.uket.domain.auth.domain;

import com.uket.domain.user.dto.UserDto;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

@RequiredArgsConstructor
public class CustomOAuth2User implements UserDetails {

    private final UserDto userDto;

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        Collection<GrantedAuthority> collection = new ArrayList<>();
        collection.add((GrantedAuthority) userDto::role);

        return collection;
    }

    @Override
    public String getUsername() {
        return Long.toString(userDto.memberId());
    }

    public String getName(){
        return userDto.name();
    }

    public Long getMemberId() {
        return userDto.memberId();
    }

    public Boolean isRegistered() {
        return userDto.isRegistered();
    }

    @Override
    public String getPassword() {
        return "password";
    }

    @Override
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    public boolean isAccountNonLocked() {
        return true;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    public boolean isEnabled() {
        return true;
    }
}

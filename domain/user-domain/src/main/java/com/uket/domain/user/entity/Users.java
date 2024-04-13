package com.uket.domain.user.entity;

import com.uket.domain.core.entity.BaseEntity;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Users extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "user_id")
    private Long id;

    private String name;
    private String email;
    @Enumerated(EnumType.STRING)
    private Platform platform;
    private String platformId;
    @Enumerated(EnumType.STRING)
    private UserRole role;
    private Boolean isRegistered;

    private String phoneNumber;

    public void updateEmailAndName(String email, String name) {
        this.email = email;
        this.name = name;
    }
}

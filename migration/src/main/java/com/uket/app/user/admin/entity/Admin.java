package com.uket.app.user.admin.entity;

import com.uket.app.user.admin.enums.AdminRole;
import com.uket.domain.university.entity.University;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
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
public class Admin {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "admin_id")
    private Long id;

    @ManyToOne
    @JoinColumn(name = "university_id")
    private University university;

    @Enumerated(EnumType.STRING)
    private AdminRole role;

    private String email;
    private String password;
    private String name;
    private Boolean isRegistered;
}

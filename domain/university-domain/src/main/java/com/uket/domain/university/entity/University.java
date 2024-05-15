package com.uket.domain.university.entity;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.university.exception.UniversityException;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Getter
@Builder
@AllArgsConstructor
@Table(uniqueConstraints = {
        @UniqueConstraint(columnNames = {"name"})
})
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class University {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "university_id")
    private Long id;
    private String name;
    private String emailPostFix;
    private String logoPath;
    private Long currentEvent;

    public void validateUniversityEmail(String universityEmail) {
        if(universityEmail == null || !universityEmail.contains(this.getEmailPostFix())){
            throw new UniversityException(ErrorCode.NOT_MATCH_UNIVERSITY_EMAIL);
        }
    }
}

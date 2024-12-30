package com.uket.domain.terms.entity;


import com.uket.core.exception.BaseException;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.core.entity.BaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
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
public class Terms extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "name")
    private String name;

    @Enumerated(EnumType.STRING)
    @Column(name = "type")
    private TermsType type;

    @JoinColumn(name = "documnet_no")
    private Long documentNo;

    @Column(name = "is_active")
    private Boolean isActive;

    public void checkMandatory(Boolean isAgreed) {
        if (type != TermsType.MANDATORY) {
            return;
        }

        if(Boolean.FALSE.equals(isAgreed)){
            throw new BaseException(ErrorCode.NOT_VALID_TERMS_AGREEMENT);
        }
    }
}

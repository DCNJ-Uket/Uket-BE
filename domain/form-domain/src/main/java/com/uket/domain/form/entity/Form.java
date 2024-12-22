package com.uket.domain.form.entity;

import com.uket.domain.core.entity.BaseEntity;
import com.uket.domain.user.entity.Users;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import java.util.List;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
public class Form extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "form_id")
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "survey_id")
    private Survey survey;

    @Enumerated(value = EnumType.STRING)
    private FormType formType;

    @OneToMany(mappedBy = "form_id")
    private List<Option> options;

    private String question;
    private Integer maxLength;

    public boolean containsInOptions(int index) {
        int optionCount = this.options.size();
        return index < 0 || index > optionCount;
    }

    public boolean isOverMaxLength(int length) {
        return length > maxLength;
    }
}

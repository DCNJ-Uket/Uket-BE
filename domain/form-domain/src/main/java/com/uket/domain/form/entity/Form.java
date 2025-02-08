package com.uket.domain.form.entity;

import com.uket.domain.core.entity.BaseEntity;
import jakarta.persistence.Column;
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
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
@Getter
public class Form extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "form_id")
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "survey_id")
    private Survey survey;

    @Column(columnDefinition = "varchar(255)")
    @Enumerated(value = EnumType.STRING)
    private FormType formType;

    @OneToMany(mappedBy = "form")
    private List<Options> options;

    private String question;
    private Integer maxLength;

    private Boolean isNecessary;

    public boolean containsInOptions(String response) {
        //true -> contains
        return options.stream()
            .anyMatch(option -> option.getValue().equals(response));
    }

    public boolean isOverMaxLength(int length) {
        return length > maxLength;
    }
}
